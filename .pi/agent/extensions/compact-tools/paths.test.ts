import assert from "node:assert/strict";
import test from "node:test";
import { homedir } from "node:os";
import { displayPath, shortenPath } from "./paths.ts";

const HOME = homedir();

test("short paths pass through untouched", () => {
	assert.equal(shortenPath("src/index.ts"), "src/index.ts");
	assert.equal(shortenPath("~/stash/notes/foo.org"), "~/stash/notes/foo.org");
});

test("collapses package-store scaffolding to scope/name/tail", () => {
	const store =
		"~/.local/share/pnpm/store/v11/links/@earendil-works/pi-coding-agent/0.84.2/" +
		"e2d07cad42d607702b77a119736421bc43062e4f57a18015833c0a551f629d2c/node_modules/" +
		"@earendil-works/pi-coding-agent/dist/modes/interactive/interactive-mode.js";
	const short = shortenPath(store);
	assert.ok(short.length < store.length / 3, short);
	assert.ok(short.startsWith("…/"), short);
	// the parts worth reading survive: package name and the file's own path
	assert.ok(short.includes("pi-coding-agent"), short);
	assert.ok(short.endsWith("dist/modes/interactive/interactive-mode.js"), short);
	// the parts worth dropping do not
	assert.ok(!short.includes("e2d07cad"), short);
	assert.ok(!short.includes("pnpm"), short);
});

test("elides the head of a deep path with no node_modules", () => {
	const deep = `~/${"segment/".repeat(20)}file.ts`;
	const short = shortenPath(deep);
	assert.ok(short.startsWith("…/"), short);
	assert.ok(short.length <= 74, short); // 72 budget + the "…/" it pays for
	assert.ok(short.endsWith("/file.ts"), short);
});

test("never elides away the filename itself", () => {
	const long = `~/${"x".repeat(300)}.ts`;
	assert.equal(shortenPath(long), `…/${"x".repeat(300)}.ts`);
});

test("a bare node_modules tail is left alone", () => {
	assert.equal(shortenPath("/a/node_modules/"), "/a/node_modules/");
});

test("displayPath prefers cwd-relative, then ~, then shortened", () => {
	assert.equal(displayPath("/repo/src/a.ts", "/repo"), "src/a.ts");
	assert.equal(displayPath("/repo", "/repo"), ".");
	assert.equal(displayPath(undefined, "/repo"), ".");
	assert.equal(displayPath(`${HOME}/notes/a.org`, "/repo"), "~/notes/a.org");
	assert.equal(displayPath("/etc/hosts", "/repo"), "/etc/hosts");
});

test("displayPath expands a leading tilde instead of resolving it as a dir", () => {
	assert.equal(displayPath("~/notes/a.org", "/repo"), "~/notes/a.org");
	assert.equal(displayPath("~", "/repo"), "~");
	// inside cwd it becomes relative, which is shorter still
	assert.equal(displayPath("~/repo/src/a.ts", `${HOME}/repo`), "src/a.ts");
});
