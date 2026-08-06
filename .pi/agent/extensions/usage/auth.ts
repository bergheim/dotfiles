import fs from "node:fs";
import os from "node:os";
import path from "node:path";

export type Credential = { token: string } | { stale: string };

export const AUTH_FILE = path.join(os.homedir(), ".pi", "agent", "auth.json");
export const ANTIGRAVITY_TOKEN_FILE = path.join(
  os.homedir(), ".gemini", "antigravity-cli", "antigravity-oauth-token",
);

type ReadResult =
  | { ok: Record<string, any> }
  | { missing: true }
  | { corrupt: true };

// Read-only by design. Refreshing here would race pi's own refresh on a mount
// shared by the host and every container; a rotated token written back stale
// logs every pi on that mount out.
function readJson(file: string): ReadResult {
  let raw: string;
  try {
    raw = fs.readFileSync(file, "utf-8");
  } catch (err: any) {
    return err?.code === "ENOENT" ? { missing: true } : { corrupt: true };
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch {
    return { corrupt: true };
  }
  // A credential store must be a plain object: null, arrays, and scalars all
  // parse fine as JSON but crash the `entry.foo` lookups below if allowed
  // through.
  if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed)) {
    return { corrupt: true };
  }
  return { ok: parsed as Record<string, any> };
}

// The only thing standing between a malformed store and fetchOne's
// `credential.token.slice(...)` crashing downstream: no reader may hand back
// `{token}` unless the value is a genuine, non-empty string.
function isLiveToken(value: unknown): value is string {
  return typeof value === "string" && value.length > 0;
}

function fromAuthFile(
  provider: string, authFile: string, nowMs: number,
): Credential {
  const result = readJson(authFile);
  if ("missing" in result) return { stale: "no credential" };
  if ("corrupt" in result) return { stale: "unreadable" };
  const entry = result.ok[provider];
  if (!isLiveToken(entry?.access)) return { stale: "no credential" };
  // Real codex/anthropic entries always carry a numeric `expires` (unlike
  // Antigravity's file, which has none by design). An absent field is as
  // untrustworthy as a malformed one here, so both fail safe as expired
  // rather than being read as a live, never-expiring token.
  if (typeof entry.expires !== "number" || entry.expires <= nowMs) {
    return { stale: "expired" };
  }
  return { token: entry.access };
}

export function readCodexCredential(
  authFile = AUTH_FILE, nowMs = Date.now(),
): Credential {
  return fromAuthFile("openai-codex", authFile, nowMs);
}

export function readAnthropicCredential(
  authFile = AUTH_FILE, nowMs = Date.now(),
): Credential {
  return fromAuthFile("anthropic", authFile, nowMs);
}

// Antigravity has two possible stores. Preferred: auth.json's
// `google-antigravity` entry — pi's own credential store, populated when
// Antigravity is logged in through pi, same shape as codex/anthropic.
// Fallback: the standalone `agy` CLI's own token file, a different shape
// with no expiry field, read read-only purely for quota visibility when pi
// has no Antigravity login of its own.
export function readAntigravityCredential(
  tokenFile = ANTIGRAVITY_TOKEN_FILE, authFile = AUTH_FILE,
): Credential {
  const authResult = readJson(authFile);
  if ("ok" in authResult) {
    const access = authResult.ok["google-antigravity"]?.access;
    if (isLiveToken(access)) return { token: access };
  }

  const result = readJson(tokenFile);
  if ("missing" in result) return { stale: "no credential" };
  if ("corrupt" in result) return { stale: "unreadable" };
  const accessToken = result.ok.token?.access_token;
  return isLiveToken(accessToken)
    ? { token: accessToken }
    : { stale: "no credential" };
}
