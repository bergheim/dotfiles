/**
 * compact-tools — three-level tool density
 *
 * Modes (cycle with ctrl+b):
 *   title   — built-in header only (command/path)
 *   preview — header + stock-ish collapsed preview (~5–10 lines)
 *   full    — header + entire tool output
 *
 * renderCall matches stock headers (bash: `$ command`).
 * renderResult is overridden for density. Execution stays stock.
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
} from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { homedir } from "node:os";
import { relative, resolve } from "node:path";

const HOME = homedir();
const EXPANDED_MAX_LINES = 400;
/** Matches stock bash tool collapsed preview. */
const PREVIEW_BASH_LINES = 5;
/** Matches stock generic tool collapsed preview. */
const PREVIEW_GENERIC_LINES = 10;
const PREVIEW_DIFF_LINES = 30;

type Density = "title" | "preview" | "full";
const DENSITY_ORDER: Density[] = ["title", "preview", "full"];
const DENSITY_LABEL: Record<Density, string> = {
	title: "title (1-line)",
	preview: "preview (PI-like)",
	full: "full",
};

let density: Density = "title";

type Theme = {
	fg: (key: string, text: string) => string;
	bold: (text: string) => string;
};

function truncate(s: string, max: number): string {
	const one = s.replace(/\s+/g, " ").trim();
	if (one.length <= max) return one;
	return one.slice(0, max - 1) + "…";
}

function textContent(result: { content: Array<{ type: string; text?: string }> }): string {
	return result.content
		.filter((c): c is { type: "text"; text: string } => c.type === "text" && typeof c.text === "string")
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

/** Stock-ish path display (relative to cwd, else ~). */
function displayPath(path: string | undefined, cwd: string): string {
	if (!path) return ".";
	const abs = resolve(cwd, path);
	if (abs === cwd) return ".";
	const rel = relative(cwd, abs);
	if (rel && !rel.startsWith("..") && !rel.startsWith("/")) return rel;
	if (abs.startsWith(HOME + "/")) return "~" + abs.slice(HOME.length);
	return path;
}

/** Reuse last Text component like stock renderers. */
function callText(context: { lastComponent?: unknown }, line: string): Text {
	const text = (context.lastComponent as Text | undefined) ?? new Text("", 0, 0);
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
function bodyForMode(theme: Theme, output: string, previewLines: number): string {
	if (density === "title") return "";
	if (!output) {
		return density === "full" ? "\n" + theme.fg("muted", "(no output)") : "";
	}

	const lines = output.split("\n");
	const limit = density === "preview" ? previewLines : EXPANDED_MAX_LINES;
	// preview: last N lines (stock bash shows tail); full: from start
	const slice =
		density === "preview" ? lines.slice(Math.max(0, lines.length - previewLines)) : lines.slice(0, limit);

	let body = "\n" + slice.map((l) => theme.fg("toolOutput", l)).join("\n");
	const hidden = lines.length - slice.length;
	if (hidden > 0) {
		body +=
			"\n" +
			theme.fg(
				"muted",
				density === "preview" ? `… ${hidden} earlier lines` : `… ${hidden} more lines`,
			);
	}
	return body;
}

function refreshTools(ctx: ExtensionContext) {
	// setToolsExpanded no-ops when value unchanged; flip to force every tool row to re-render.
	const target = density === "full";
	ctx.ui.setToolsExpanded(!target);
	ctx.ui.setToolsExpanded(target);
	ctx.ui.setStatus("compact-tools", ctx.ui.theme.fg("muted", `tools:${density}`));
	ctx.ui.notify(`Tool density: ${DENSITY_LABEL[density]}`, "info");
}

function setDensity(ctx: ExtensionContext, next: Density) {
	density = next;
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
	if (s === "preview" || s === "pi" || s === "default" || s === "2") return "preview";
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
					ctx.ui.notify(`Unknown density "${args.trim()}". Use title, preview, or full.`, "warning");
					return;
				}
				cycleDensity(ctx);
				return;
			}
			setDensity(ctx, next);
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		ctx.ui.setStatus("compact-tools", ctx.ui.theme.fg("muted", `tools:${density}`));
	});

	// --- bash ---
	pi.registerTool({
		name: "bash",
		label: "bash",
		description: "Execute a bash command in the current working directory.",
		parameters: createBashTool(process.cwd()).parameters,
		promptSnippet: "Execute bash commands (ls, grep, find, etc.)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createBashTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		// Match stock formatBashCall: bold toolTitle `$ command`
		renderCall(args, theme, context) {
			const cmd = args?.command != null ? String(args.command) : "";
			const commandDisplay = cmd || theme.fg("toolOutput", "...");
			let line = theme.fg("toolTitle", theme.bold(`$ ${commandDisplay}`));
			if (args?.timeout != null) {
				line += theme.fg("muted", ` (timeout ${args.timeout}s)`);
			}
			return callText(context, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … running"), 0, 0);
			}

			const output = textContent(result);
			const details = result.details as BashToolDetails | undefined;
			const lines = lineCount(output);
			const failed = context.isError;
			const exitMatch = output.match(/exited with code (\d+)/i) ?? output.match(/exit code[: ]+(\d+)/i);
			const exitCode = exitMatch ? Number(exitMatch[1]) : failed ? "?" : 0;

			if (density === "title") {
				if (failed) return new Text(statusErr(theme, `  ✗ exit ${exitCode}`), 0, 0);
				return emptyResult();
			}

			let status = failed ? statusErr(theme, `  ✗ exit ${exitCode}`) : statusOk(theme, "  ✓");
			status += theme.fg("muted", ` ${lines} lines`);
			if (details?.truncation?.truncated) status += theme.fg("warning", " truncated");

			return new Text(status + bodyForMode(theme, output, PREVIEW_BASH_LINES), 0, 0);
		},
	});

	// --- read ---
	pi.registerTool({
		name: "read",
		label: "read",
		description: "Read a file.",
		parameters: createReadTool(process.cwd()).parameters,
		promptSnippet: "Read file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createReadTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			let line = `${theme.fg("toolTitle", theme.bold("read"))} ${path}`;
			if (args?.offset != null || args?.limit != null) {
				const parts: string[] = [];
				if (args?.offset != null) parts.push(`offset=${args.offset}`);
				if (args?.limit != null) parts.push(`limit=${args.limit}`);
				line += theme.fg("muted", ` (${parts.join(", ")})`);
			}
			return callText(context, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … reading"), 0, 0);
			}
			if (context.isError) {
				const msg = truncate(textContent(result).split("\n")[0] || "error", 80);
				return new Text(statusErr(theme, `  ✗ ${msg}`), 0, 0);
			}

			const content = result.content[0];
			if (content?.type === "image") {
				return density === "title" ? emptyResult() : new Text(statusOk(theme, "  ✓ image"), 0, 0);
			}

			const output = textContent(result);
			const details = result.details as ReadToolDetails | undefined;
			const lines = lineCount(output);

			if (density === "title") return emptyResult();

			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${lines} lines`);
			if (details?.truncation?.truncated) {
				status += theme.fg("warning", ` of ${details.truncation.totalLines}`);
			}
			return new Text(status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES), 0, 0);
		},
	});

	// --- grep ---
	pi.registerTool({
		name: "grep",
		label: "grep",
		description: "Search file contents for patterns.",
		parameters: createGrepTool(process.cwd()).parameters,
		promptSnippet: "Search file contents for patterns (respects .gitignore)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createGrepTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			// Shell-prompt style: $ grep pattern [path] [glob]
			const pattern = args?.pattern != null ? String(args.pattern) : "";
			const path = args?.path != null ? displayPath(String(args.path), cwdOf(context)) : undefined;
			const parts = ["grep", pattern || "..."];
			if (path) parts.push(path);
			if (args?.glob) parts.push(String(args.glob));
			return callText(context, theme.fg("toolTitle", theme.bold(`$ ${parts.join(" ")}`)));
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … searching"), 0, 0);
			}
			if (context.isError) {
				return new Text(statusErr(theme, `  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`), 0, 0);
			}

			const output = textContent(result);
			const details = result.details as GrepToolDetails | undefined;
			if (density === "title") return emptyResult();

			const hits = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${hits} hits`);
			if (details?.matchLimitReached) status += theme.fg("warning", " limit");
			if (details?.truncation?.truncated || details?.linesTruncated) status += theme.fg("warning", " trunc");
			return new Text(status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES), 0, 0);
		},
	});

	// --- find ---
	pi.registerTool({
		name: "find",
		label: "find",
		description: "Find files by glob pattern.",
		parameters: createFindTool(process.cwd()).parameters,
		promptSnippet: "Find files by glob pattern (respects .gitignore)",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createFindTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			// Shell-prompt style: $ find <path> -name pattern  (approx)
			const pattern = args?.pattern != null ? String(args.pattern) : "...";
			const path = displayPath(args?.path, cwdOf(context));
			return callText(context, theme.fg("toolTitle", theme.bold(`$ find ${path} ${pattern}`)));
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … finding"), 0, 0);
			}
			if (context.isError) {
				return new Text(statusErr(theme, `  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`), 0, 0);
			}

			const output = textContent(result);
			const details = result.details as FindToolDetails | undefined;
			if (density === "title") return emptyResult();

			const n = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${n} files`);
			if (details?.resultLimitReached) status += theme.fg("warning", " limit");
			return new Text(status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES), 0, 0);
		},
	});

	// --- ls ---
	pi.registerTool({
		name: "ls",
		label: "ls",
		description: "List directory contents.",
		parameters: createLsTool(process.cwd()).parameters,
		promptSnippet: "List directory contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createLsTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			// Shell-prompt style — what you type at a prompt, e.g. `$ ls ~/Downloads`
			const raw = args?.path != null ? String(args.path) : ".";
			// Prefer the arg as the model wrote it (keeps ~); fall back to displayPath.
			const path = raw.startsWith("~") || raw.startsWith("/") ? raw : displayPath(raw, cwdOf(context));
			let line = theme.fg("toolTitle", theme.bold(`$ ls ${path}`));
			if (args?.limit != null) line += theme.fg("muted", ` (limit ${args.limit})`);
			return callText(context, line);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … listing"), 0, 0);
			}
			if (context.isError) {
				return new Text(statusErr(theme, `  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`), 0, 0);
			}

			const output = textContent(result);
			const details = result.details as LsToolDetails | undefined;
			if (density === "title") return emptyResult();

			const n = output.trim() ? lineCount(output) : 0;
			let status = statusOk(theme, "  ✓") + theme.fg("muted", ` ${n} entries`);
			if (details?.entryLimitReached) status += theme.fg("warning", " limit");
			return new Text(status + bodyForMode(theme, output, PREVIEW_GENERIC_LINES), 0, 0);
		},
	});

	// --- edit ---
	pi.registerTool({
		name: "edit",
		label: "edit",
		description: "Edit a file.",
		parameters: createEditTool(process.cwd()).parameters,
		promptSnippet: "Edit file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createEditTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			return callText(context, `${theme.fg("toolTitle", theme.bold("edit"))} ${path}`);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … editing"), 0, 0);
			}
			if (context.isError) {
				return new Text(statusErr(theme, `  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`), 0, 0);
			}

			const details = result.details as EditToolDetails | undefined;
			if (!details?.diff) {
				return density === "title" ? emptyResult() : new Text(statusOk(theme, "  ✓ applied"), 0, 0);
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
				if (line.startsWith("+") && !line.startsWith("+++")) body += "\n" + theme.fg("toolDiffAdded", line);
				else if (line.startsWith("-") && !line.startsWith("---")) body += "\n" + theme.fg("toolDiffRemoved", line);
				else body += "\n" + theme.fg("toolDiffContext", line);
			}
			if (diffLines.length > max) {
				body += "\n" + theme.fg("muted", `… ${diffLines.length - max} more diff lines`);
			}
			return new Text(status + body, 0, 0);
		},
	});

	// --- write ---
	pi.registerTool({
		name: "write",
		label: "write",
		description: "Write a file.",
		parameters: createWriteTool(process.cwd()).parameters,
		promptSnippet: "Write file contents",

		async execute(toolCallId, params, signal, onUpdate, ctx) {
			return createWriteTool(cwdOf(ctx)).execute(toolCallId, params, signal, onUpdate, ctx);
		},

		renderCall(args, theme, context) {
			const path = displayPath(args?.path ?? args?.file_path, cwdOf(context));
			return callText(context, `${theme.fg("toolTitle", theme.bold("write"))} ${path}`);
		},

		renderResult(result, { isPartial }, theme, context) {
			if (isPartial) {
				return density === "title" ? emptyResult() : new Text(statusRun(theme, "  … writing"), 0, 0);
			}
			if (context.isError) {
				return new Text(statusErr(theme, `  ✗ ${truncate(textContent(result).split("\n")[0] || "error", 80)}`), 0, 0);
			}
			return density === "title" ? emptyResult() : new Text(statusOk(theme, "  ✓ written"), 0, 0);
		},
	});
}
