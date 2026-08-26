import { homedir } from "node:os";
import { relative, resolve } from "node:path";

const HOME = homedir();

/** Longest path a tool-call header shows before the head gets elided. */
const PATH_MAX = 72;

/**
 * Shorten a path the way an editor breadcrumb does: drop whole leading
 * components until the rest fits, and mark the cut with `…/`. The tail says
 * which file; the head of an over-long path is almost always boilerplate.
 *
 * Same family as fish's `prompt_pwd` and zsh's shrink-path, but cutting whole
 * components instead of abbreviating each one to a letter — `~/.l/s/p/s/v11`
 * is short and unreadable, which is the wrong trade for a tool-call header.
 */
export function shortenPath(path: string): string {
	// Everything before the last node_modules/ is package-store scaffolding —
	// pnpm content hashes, virtual-store dirs, the package name repeated —
	// that says nothing about the file. What follows is scope/name/…/file,
	// which is the most informative part of the whole string.
	const nm = path.lastIndexOf("/node_modules/");
	const rest = nm === -1 ? path : path.slice(nm + "/node_modules/".length);
	if (!rest) return path;

	let parts = rest.split("/");
	let elided = nm !== -1;
	// "…/" costs two columns, so it counts against the budget it creates.
	while (
		parts.length > 1 &&
		parts.join("/").length + (elided ? 2 : 0) > PATH_MAX
	) {
		parts = parts.slice(1);
		elided = true;
	}
	return elided ? `…/${parts.join("/")}` : parts.join("/");
}

/** Render a tool argument as the shortest path that still says where it is. */
export function displayPath(path: string | undefined, cwd: string): string {
	if (!path) return ".";
	// The model writes "~/x" as often as an absolute path, and resolve() would
	// read the tilde as a directory name and hang it off cwd.
	const expanded =
		path === "~" ? HOME : path.startsWith("~/") ? HOME + path.slice(1) : path;
	const abs = resolve(cwd, expanded);
	if (abs === cwd) return ".";
	const rel = relative(cwd, abs);
	if (rel && !rel.startsWith("..") && !rel.startsWith("/"))
		return shortenPath(rel);
	if (abs === HOME || abs.startsWith(`${HOME}/`))
		return shortenPath(`~${abs.slice(HOME.length)}`);
	return shortenPath(expanded);
}
