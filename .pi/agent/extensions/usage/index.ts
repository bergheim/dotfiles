import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { fetchAll, type ProviderStatus } from "./core.ts";
import { renderBar, barColor, formatDuration } from "./render.ts";

function line(status: ProviderStatus): string {
  if ("stale" in status) return `${status.name}: — (${status.stale})`;
  const { sessionPercent, weeklyPercent, resetsInSeconds } = status.usage;
  const reset = resetsInSeconds === null
    ? ""
    : `  resets ${formatDuration(resetsInSeconds)}`;
  return (
    `${status.name}: ${renderBar(sessionPercent)} ${Math.round(sessionPercent)}%` +
    `  week ${Math.round(weeklyPercent)}%${reset}`
  );
}

// barColor speaks red/yellow/green; the theme only knows semantic slots.
const TONE = { red: "error", yellow: "warning", green: "success" } as const;

// Uncolored text only: width budgeting (renderFooterLines) measures this,
// never the ANSI-wrapped result, so a color escape sequence can never be
// sliced in half by a width cut.
function plainSegment(status: ProviderStatus): string {
  if ("stale" in status) return `${status.name} —`;
  const bar = renderBar(status.usage.sessionPercent, 6);
  return `${status.name} ${bar}`;
}

function footerLine(theme: { fg(color: string, text: string): string }, status: ProviderStatus): string {
  const segment = plainSegment(status);
  if ("stale" in status) return theme.fg("dim", segment);
  return theme.fg(TONE[barColor(status.usage.sessionPercent)], segment);
}

// pi's Component.render(width) contract requires every returned line to fit
// the viewport; a narrow terminal can't always show all providers. We drop
// whole columns rather than wrap (footer must stay one line) or truncate
// inside a colored segment (would cut an ANSI escape in half and corrupt the
// terminal). Included/omitted is decided on the plain, uncolored text, then
// theme.fg is applied only to segments already known to fit.
export function renderFooterLines(
  theme: { fg(color: string, text: string): string },
  statuses: ProviderStatus[],
  width: number,
): string[] {
  const safeWidth = Math.max(0, width);

  if (statuses.length === 0) {
    return [theme.fg("dim", "usage: loading…".slice(0, safeWidth))];
  }

  const sep = "  ";
  const included: ProviderStatus[] = [];
  let used = 0;
  for (const status of statuses) {
    const seg = plainSegment(status);
    const next = used + (included.length > 0 ? sep.length : 0) + seg.length;
    if (next > safeWidth) break;
    included.push(status);
    used = next;
  }

  if (included.length === 0) return [""];

  const parts = included.map((s) => footerLine(theme, s));
  const omitted = statuses.length - included.length;
  if (omitted > 0) {
    const marker = `+${omitted}`;
    if (used + sep.length + marker.length <= safeWidth) {
      parts.push(theme.fg("dim", marker));
    }
  }
  return [parts.join(sep)];
}

export default function (pi: ExtensionAPI) {
  pi.registerCommand("usage", {
    description: "Show provider quota for Codex, Claude, and Antigravity",
    handler: async (_args, ctx) => {
      const statuses = await fetchAll();
      ctx.ui.notify(statuses.map(line).join("\n"), "info");
    },
  });

  // fetchAll degrades every failure to a stale marker, so the footer never
  // has to distinguish "not fetched yet" from "provider unreachable" beyond
  // this initial empty-array loading state.
  let statuses: ProviderStatus[] = [];
  let requestRender: (() => void) | undefined;

  async function refresh(): Promise<void> {
    statuses = await fetchAll();
    requestRender?.();
  }

  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.setFooter((tui, theme) => {
      requestRender = () => tui.requestRender();
      return {
        invalidate() {},
        render(width: number): string[] {
          return renderFooterLines(theme, statuses, width);
        },
      };
    });
    await refresh();
  });

  // Quota moves as turns run; re-fetch before each one. fetchAll's own
  // cache (core.ts, 60s TTL) keeps this from hammering provider APIs.
  // turn_start sits inline in pi's sequential turn pipeline, so this must
  // not await: the footer already repaints itself via requestRender() once
  // refresh() lands. fetchAll degrades every failure internally, but the
  // catch here is cheap insurance against an unhandled rejection wedging
  // the process if that guarantee is ever broken.
  pi.on("turn_start", () => {
    refresh().catch(() => {});
  });
}
