/**
 * compact-tools — three-level tool density
 *
 * Modes (cycle with ctrl+b):
 *   title   — header only, self-shell (no Box pad). Still one row per call —
 *             pi always inserts a blank line; cannot stack calls onto one line.
 *   preview — header + stock-ish collapsed preview (~5–10 lines)
 *   full    — header + entire tool output
 *
 * renderCall draws an icon-led one-line header; long paths and free-text args
 * are shortened so it stays one line. renderResult is overridden for density.
 * Execution stays stock.
 *
 * Covers: bash, read, grep, find, ls, edit, write.
 * Does not affect interactive `!` bang-bash.
 */

import type {
	BashToolDetails,
	EditToolDetails,
	ExtensionAPI,
	ExtensionContext,
	FindToolDetails,
	GrepToolDetails,
	LsToolDetails,
	ReadToolDetails,
} from "@earendil-works/pi-coding-agent";
import {
	createBashTool,
	createEditTool,
	createFindTool,
	createGrepTool,
	createLsTool,
	createReadTool,
	createWriteTool,
	getAgentDir,
} from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { displayPath } from "./paths.ts";

const EXPANDED_MAX_LINES = 400;
/** Matches stock bash tool collapsed preview. */
const PREVIEW_BASH_LINES = 5;
/** Matches stock generic tool collapsed preview. */
const PREVIEW_GENERIC_LINES = 10;
const PREVIEW_DIFF_LINES = 30;
/**
 * Free-text call args — a command, a search pattern — clipped so the title
 * density keeps its one-line promise. truncate() also folds newlines, which
 * is what a heredoc or a multi-line && chain would otherwise smuggle in.
 */
const CALL_ARG_MAX = 100;

type Density = "title" | "preview" | "full";
const DENSITY_ORDER: Density[] = ["title", "preview", "full"];
const DENSITY_LABEL: Record<Density, string> = {
	title: "title (1-line)",
	preview: "preview (PI-like)",
	full: "full",
};

const SETTINGS_KEY = "compactToolsDensity";
const SETTINGS_PATH = join(getAgentDir(), "settings.json");

function loadDensity(): Density {
	try {
		const raw = JSON.parse(readFileSync(SETTINGS_PATH, "utf8"));
		const value = raw?.[SETTINGS_KEY];
		if (value === "title" || value === "preview" || value === "full")
			return value;
	} catch {
		// missing or junk settings: keep the built-in default
	}
	return "title";
}

function saveDensity(next: Density): void {
	try {
		const raw = JSON.parse(readFileSync(SETTINGS_PATH, "utf8"));
		if (!raw || typeof raw !== "object" || Array.isArray(raw)) return;
		raw[SETTINGS_KEY] = next;
		writeFileSync(SETTINGS_PATH, `${JSON.stringify(raw, null, 2)}\n`);
	} catch {
		// settings write is best-effort; density still applies this session
	}
}

let density: Density = loadDensity();

type Theme = {
	fg: (key: string, text: string) => string;
	bold: (text: string) => string;
	bg?: (key: string, text: string) => string;
};

const TOOL_ICON = {
	bash: "",
	read: "󰈙",
	grep: "",
	find: "󰈞",
	ls: "󰉋",
	edit: "",
	write: "",
} as const;

function toolTitle(theme: Theme, name: keyof typeof TOOL_ICON): string {
	return theme.fg("toolTitle", theme.bold(`${TOOL_ICON[name]} ${name}`));
}

function truncate(s: string, max: number): string {
	const one = s.replace(/\s+/g, " ").trim();
	if (one.length <= max) return one;
	return one.slice(0, max - 1) + "…";
}

function textContent(result: {
	content: Array<{ type: string; text?: string }>;
}): string {
	return result.content
		.filter(
			(c): c is { type: "text"; text: string } =>
				c.type === "text" && typeof c.text === "string",
		)
		.map((c) => c.text)
		.join("\n");
}

function lineCount(s: string): number {
	if (!s) return 0;
	return s.replace(/\n$/, "").split("\n").length;
}

function cwdOf(ctx: { cwd?: string } | undefined): string {
	return ctx?.cwd || process.cwd();
}

/** Stock tool-box bg. Same fn for title / preview / full so ctrl+b doesn't recolor. */
function toolBg(
	theme: Theme,
	context: { isPartial?: boolean; isError?: boolean },
): ((s: string) => string) | undefined {
	if (!theme.bg) return undefined;
	let slot = "toolSuccessBg";
	if (context.isPartial) slot = "toolPendingBg";
	else if (context.isError) slot = "toolErrorBg";
	return (s) => theme.bg(slot, s);
}

/** Reuse last Text component like stock renderers. */
function paintedText(
	context: {
		lastComponent?: unknown;
		isPartial?: boolean;
		isError?: boolean;
	},
	theme: Theme,
	line: string,
): Text {
	const text = (context.lastComponent as Text | undefined) ?? new Text("", 0, 0);
	text.setCustomBgFn(toolBg(theme, context));
	text.setText(line);
	return text;
}

function statusOk(theme: Theme, label: string): string {
	return theme.fg("success", label);
}
function statusErr(theme: Theme, label: string): string {
	return theme.fg("error", label);
}
function statusRun(theme: Theme, label: string): string {
	return theme.fg("warning", label);
}

/** Body under the stock header: nothing | preview | full. */
function bodyForMode(
	theme: Theme,
	output: string,
	previewLines: number,
): string {
	if (density === "title") return "";
	if (!output) {
		return density === "full" ? "\n" + theme.fg("muted", "(no output)") : "";
	}

	const lines = output.split("\n");
	const limit = density === "preview" ? previewLines : EXPANDED_MAX_LINES;
	// preview: last N lines (stock bash shows tail); full: from start
	const slice =
		density === "preview"
			? lines.slice(Math.max(0, lines.length - previewLines))
			: lines.slice(0, limit);

	let body = "\n" + slice.map((l) => theme.fg("toolOutput", l)).join("\n");
	const hidden = lines.length - slice.length;
	if (hidden > 0) {
		body +=
			"\n" +
			theme.fg(
				"muted",
				density === "preview"
					? `… ${hidden} earlier lines`
					: `… ${hidden} more lines`,
			);
	}
	return body;
}

function refreshTools(ctx: ExtensionContext) {
	// setToolsExpanded no-ops when value unchanged; flip to force every tool row to re-render.
	const target = density === "full";
	ctx.ui.setToolsExpanded(!target);
	ctx.ui.setToolsExpanded(target);
	ctx.ui.setStatus(
		"compact-tools",
		ctx.ui.theme.fg("muted", `tools:${density}`),
	);
	ctx.ui.notify(`Tool density: ${DENSITY_LABEL[density]}`, "info");
}

function setDensity(ctx: ExtensionContext, next: Density) {
	density = next;
	saveDensity(next);
	refreshTools(ctx);
}

function cycleDensity(ctx: ExtensionContext) {
	const i = DENSITY_ORDER.indexOf(density);
	setDensity(ctx, DENSITY_ORDER[(i + 1) % DENSITY_ORDER.length]);
}

function parseDensity(raw: string | undefined): Density | undefined {
	const s = raw?.trim().toLowerCase();
	if (!s) return undefined;
	if (s === "title" || s === "1" || s === "one" || s === "line") return "title";
	if (s === "preview" || s === "pi" || s === "default" || s === "2")
		return "preview";
	if (s === "full" || s === "all" || s === "3") return "full";
	return undefined;
}

/** Empty result slot — stock header alone is the title line. */
function emptyResult() {
	return new Text("", 0, 0);
}

export default function (pi: ExtensionAPI) {
	pi.registerShortcut("ctrl+b", {
		description: "Cycle tool density (title → preview → full)",
		handler: async (ctx) => {
			cycleDensity(ctx);
		},
	});

	pi.registerCommand("tools-density", {
		description: "Set tool density: title | preview | full (or cycle if no arg)",
		handler: async (args, ctx) => {
			const next = parseDensity(args);
			if (!next) {
				if (args?.trim()) {
					ctx.ui.notify(
						`Unknown density "${args.trim()}". Use title, preview, or full.`,
						"warning",
					);
					return;
				}
				cycleDensity(ctx);
				return;
			}
			setDensity(ctx, next);
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		density = loadDensity();
		ctx.ui.setToolsExpanded(density === "full");
		ctx.ui.setStatus(
			"compact-tools",
			ctx.ui.theme.fg("muted", `tools:${density}`),
		);
	});

	/** Title mode drops the Box shell. pi reads renderShell live per render, so it
	 * has to stay a getter — a spread would freeze today's value. */
	const registerTool = (tool: Parameters<typeof pi.registerTool>[0]): void =>
		pi.registerTool(
			Object.defineProperty(tool, "renderShell", {
				get: () => (density === "title" ? "self" : "default"),
				configurable: true,
			}),
		);

	// --- bash ---
	registerTool({
		name: "bash",
		label: "bash",
		description: "Execute a bash command in the current working directory.",
		parameters: createBashTool(process.cwd()).parameters,
		promptSnippet: "Execute bash commands (ls, rg, fd, etc.)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createBashTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const cmd = args?.command == null ? "" : String(args.command);
			const commandDisplay = cmd ? truncate(cmd, CALL_ARG_MAX) : "...";
			let line = `${toolTitle(theme, "bash")} ${theme.fg("toolOutput", commandDisplay)}`;
			if (args?.timeout != null) {
				line += theme.fg("muted", ` (timeout ${args.timeout}s)`);
			}
			return paintedText(context, theme, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … running"));
			}

			const output = textContent(result);
			const details = result.details as BashToolDetails | undefined;
			const lines = lineCount(output);
			const failed = context.isError;
			const exitMatch =
				output.match(/exited with code (\d+)/i) ??
				output.match(/exit code[: ]+(\d+)/i);
			const exitCode = exitMatch ? Number(exitMatch[1]) : failed ? "?" : 0;

			if (density === "title") {
				if (failed)
					return paintedText(
						context,
						theme,
						statusErr(theme, `  ✗ exit ${exitCode}`),
					);
				return emptyResult();
			}

			let status = failed
				? statusErr(theme, `  ✗ exit ${exitCode}`)
				: statusOk(theme, "  ✓");
			status += theme.fg("muted", ` ${lines} lines`);
			if (details?.truncation?.truncated)
				status += theme.fg("warning", " truncated");

			return paintedText(
				context,
				theme,
				status + bodyForMode(theme, output, PREVIEW_BASH_LINES),
			);
		},
	});

	// --- read ---
	registerTool({
		name: "read",
		label: "read",
		description: "Read a file.",
		parameters: createReadTool(process.cwd()).parameters,
		promptSnippet: "Read file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createReadTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			let line = `${toolTitle(theme, "read")} ${theme.fg("toolOutput", path)}`;
			if (args?.offset != null || args?.limit != null) {
				const parts: string[] = [];
				if (args?.offset != null) parts.push(`offset=${args.offset}`);
				if (args?.limit != null) parts.push(`limit=${args.limit}`);
				line += theme.fg("muted", ` (${parts.join(", ")})`);
			}
			return paintedText(context, theme, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … reading"));
			}
			if (context.isError) {
				const msg = truncate(textContent(result).split("\n")[0] || "error", 80);
				return paintedText(context, theme, statusErr(theme, `  ✗ ${msg}`));
			}

			const content = result.content[0];
			if (content?.type === "image") {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusOk(theme, "  ✓ image"));
			}

			const output = textContent(result);
			const details = result.details as ReadToolDetails | undefined;
			const lines = lineCount(output);

			if (density === "title") return emptyResult();

			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${lines} lines`);
			if (details?.truncation?.truncated) {
				status += theme.fg("warning", ` of ${details.truncation.totalLines}`);
			}
			return paintedText(
				context,
				theme,
				status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES),
			);
		},
	});

	// --- grep ---
	registerTool({
		name: "grep",
		label: "grep",
		description: "Search file contents for patterns.",
		parameters: createGrepTool(process.cwd()).parameters,
		promptSnippet: "Search file contents with ripgrep (respects .gitignore)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createGrepTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const pattern = args?.pattern == null ? "" : String(args.pattern);
			const path =
				args?.path == null
					? undefined
					: displayPath(String(args.path), cwdOf(context));
			const parts = [pattern ? truncate(pattern, CALL_ARG_MAX) : "..."];
			if (path) parts.push(path);
			if (args?.glob) parts.push(String(args.glob));
			return paintedText(
				context,
				theme,
				`${toolTitle(theme, "grep")} ${theme.fg("toolOutput", parts.join(" "))}`,
			);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … searching"));
			}
			if (context.isError) {
				return paintedText(
					context,
					theme,
					statusErr(
						theme,
						`  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`,
					),
				);
			}

			const output = textContent(result);
			const details = result.details as GrepToolDetails | undefined;
			if (density === "title") return emptyResult();

			const hits = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${hits} hits`);
			if (details?.matchLimitReached) status += theme.fg("warning", " limit");
			if (details?.truncation?.truncated || details?.linesTruncated)
				status += theme.fg("warning", " trunc");
			return paintedText(
				context,
				theme,
				status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES),
			);
		},
	});

	// --- find ---
	registerTool({
		name: "find",
		label: "find",
		description: "Find files by glob pattern.",
		parameters: createFindTool(process.cwd()).parameters,
		promptSnippet: "Find files with fd by glob pattern (respects .gitignore)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createFindTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const pattern =
				args?.pattern == null
					? "..."
					: truncate(String(args.pattern), CALL_ARG_MAX);
			const path = displayPath(args?.path, cwdOf(context));
			return paintedText(
				context,
				theme,
				`${toolTitle(theme, "find")} ${theme.fg("toolOutput", `${path} ${pattern}`)}`,
			);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … finding"));
			}
			if (context.isError) {
				return paintedText(
					context,
					theme,
					statusErr(
						theme,
						`  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`,
					),
				);
			}

			const output = textContent(result);
			const details = result.details as FindToolDetails | undefined;
			if (density === "title") return emptyResult();

			const n = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${n} files`);
			if (details?.resultLimitReached) status += theme.fg("warning", " limit");
			return paintedText(
				context,
				theme,
				status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES),
			);
		},
	});

	// --- ls ---
	registerTool({
		name: "ls",
		label: "ls",
		description: "List directory contents.",
		parameters: createLsTool(process.cwd()).parameters,
		promptSnippet: "List directory contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createLsTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const raw = args?.path == null ? "." : String(args.path);
			const path = displayPath(raw, cwdOf(context));
			let line = `${toolTitle(theme, "ls")} ${theme.fg("toolOutput", path)}`;
			if (args?.limit != null) line += theme.fg("muted", ` (limit ${args.limit})`);
			return paintedText(context, theme, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … listing"));
			}
			if (context.isError) {
				return paintedText(
					context,
					theme,
					statusErr(
						theme,
						`  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`,
					),
				);
			}

			const output = textContent(result);
			const details = result.details as LsToolDetails | undefined;
			if (density === "title") return emptyResult();

			const n = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${n} entries`);
			if (details?.entryLimitReached) status += theme.fg("warning", " limit");
			return paintedText(
				context,
				theme,
				status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES),
			);
		},
	});

	// --- edit ---
	registerTool({
		name: "edit",
		label: "edit",
		description: "Edit a file.",
		parameters: createEditTool(process.cwd()).parameters,
		promptSnippet: "Edit file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createEditTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			return paintedText(
				context,
				theme,
				`${toolTitle(theme, "edit")} ${theme.fg("toolOutput", path)}`,
			);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … editing"));
			}
			if (context.isError) {
				return paintedText(
					context,
					theme,
					statusErr(
						theme,
						`  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`,
					),
				);
			}

			const details = result.details as EditToolDetails | undefined;
			if (!details?.diff) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusOk(theme, "  ✓ applied"));
			}

			const diffLines = details.diff.split("\n");
			let additions = 0;
			let removals = 0;
			for (const line of diffLines) {
				if (line.startsWith("+") && !line.startsWith("+++")) additions++;
				if (line.startsWith("-") && !line.startsWith("---")) removals++;
			}

			if (density === "title") return emptyResult();

			const status =
				statusOk(theme, "  ✓ ") +
				theme.fg("success", `+${additions}`) +
				theme.fg("muted", "/") +
				theme.fg("error", `-${removals}`);

			const max = density === "preview" ? PREVIEW_DIFF_LINES : 120;
			let body = "";
			for (const line of diffLines.slice(0, max)) {
				if (line.startsWith("+") && !line.startsWith("+++"))
					body += "\n" + theme.fg("toolDiffAdded", line);
				else if (line.startsWith("-") && !line.startsWith("---"))
					body += "\n" + theme.fg("toolDiffRemoved", line);
				else body += "\n" + theme.fg("toolDiffContext", line);
			}
			if (diffLines.length > max) {
				body +=
					"\n" + theme.fg("muted", `… ${diffLines.length - max} more diff lines`);
			}
			return paintedText(context, theme, status + body);
		},
	});

	// --- write ---
	registerTool({
		name: "write",
		label: "write",
		description: "Write a file.",
		parameters: createWriteTool(process.cwd()).parameters,
		promptSnippet: "Write file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createWriteTool(cwdOf(ctx)).execute(
				toolCallId,
				params,
				signal,
				onUpdate,
				ctx,
			);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			return paintedText(
				context,
				theme,
				`${toolTitle(theme, "write")} ${theme.fg("toolOutput", path)}`,
			);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title"
					? emptyResult()
					: paintedText(context, theme, statusRun(theme, "  … writing"));
			}
			if (context.isError) {
				return paintedText(
					context,
					theme,
					statusErr(
						theme,
						`  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`,
					),
				);
			}
			return density === "title"
				? emptyResult()
				: paintedText(context, theme, statusOk(theme, "  ✓ written"));
		},
	});
}
