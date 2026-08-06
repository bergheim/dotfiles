export type Usage = {
  sessionPercent: number;
  weeklyPercent: number;
  resetsInSeconds: number | null;
};

function num(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) ? value : null;
}

// Ported from the reference's readPercentCandidate. Providers disagree on
// whether "used" is 0-100 or 0-1; non-integer values in [0,1] are treated as
// fractions and rescaled. 0 and 1 are integers, so they pass through as
// literal 1% (not rescaled to 100%) — the boundary the reference chose for
// the genuinely ambiguous case. Anything outside [0,100] is rejected rather
// than clamped, so a bogus 250 doesn't render as a full bar.
function percent(value: unknown): number | null {
  if (typeof value !== "number" || !Number.isFinite(value)) return null;
  if (value >= 0 && value <= 1) {
    return Number.isInteger(value) ? value : value * 100;
  }
  return value >= 0 && value <= 100 ? value : null;
}

function obj(value: unknown): Record<string, any> | null {
  return value && typeof value === "object" && !Array.isArray(value)
    ? (value as Record<string, any>)
    : null;
}

// chatgpt.com/backend-api/wham/usage:
// { rate_limit: { primary_window: { used_percent, reset_after_seconds },
//                  secondary_window: { used_percent } } }
export function parseCodexUsage(payload: unknown): Usage | null {
  const rateLimit = obj(obj(payload)?.rate_limit);
  if (!rateLimit) return null;
  const primary = obj(rateLimit.primary_window);
  const session = percent(primary?.used_percent);
  if (session === null) return null;
  const secondary = obj(rateLimit.secondary_window);
  return {
    sessionPercent: session,
    weeklyPercent: percent(secondary?.used_percent) ?? session,
    resetsInSeconds: num(primary?.reset_after_seconds),
  };
}

function secondsUntil(isoDate: string, nowMs: number): number | null {
  const target = new Date(isoDate).getTime();
  if (!Number.isFinite(target)) return null;
  return Math.max(0, Math.round((target - nowMs) / 1000));
}

// api.anthropic.com/api/oauth/usage:
// { five_hour: { utilization, resets_at (ISO string) },
//   seven_day: { utilization, resets_at } }
// Unrelated to Codex's rate_limit.*_window shape, so this does not delegate
// to parseCodexUsage. resets_at is an absolute timestamp rather than a
// duration, so nowMs is accepted (defaulting live, fixed in tests) to
// convert it to seconds-until-reset.
export function parseAnthropicUsage(payload: unknown, nowMs = Date.now()): Usage | null {
  const body = obj(payload);
  if (!body) return null;
  const fiveHour = obj(body.five_hour);
  const session = percent(fiveHour?.utilization);
  if (session === null) return null;
  const sevenDay = obj(body.seven_day);
  const resetsAt = typeof fiveHour?.resets_at === "string" ? fiveHour.resets_at : null;
  return {
    sessionPercent: session,
    weeklyPercent: percent(sevenDay?.utilization) ?? session,
    resetsInSeconds: resetsAt ? secondsUntil(resetsAt, nowMs) : null,
  };
}

function usedPercent(bucket: unknown): number | null {
  const remaining = num(obj(bucket)?.remainingFraction);
  if (remaining === null) return null;
  return (1 - Math.max(0, Math.min(1, remaining))) * 100;
}

function mostUsed(buckets: unknown[]): number | null {
  let best: number | null = null;
  for (const bucket of buckets) {
    const used = usedPercent(bucket);
    if (used !== null && (best === null || used > best)) best = used;
  }
  return best;
}

// cloudcode-pa.googleapis.com/v1internal:retrieveUserQuota:
// { buckets: [{ tokenType, modelId, remainingFraction }] } — not the
// top-level `quotaBuckets` originally guessed. Buckets are filtered to
// tokenType "REQUESTS" (falling back to all buckets when none match), then
// split into gemini-pro (session) and gemini-flash (weekly) groups,
// mirroring the gemini branch of the reference's per-model bucket
// selection. The antigravity branch also weighs claude-model buckets, which
// needs a provider hint this single-arg parser doesn't take.
export function parseGoogleQuota(payload: unknown): Usage | null {
  const buckets = obj(payload)?.buckets;
  if (!Array.isArray(buckets) || buckets.length === 0) return null;

  const requestBuckets = buckets.filter(
    (b) => String(obj(b)?.tokenType ?? "").toUpperCase() === "REQUESTS",
  );
  const pool = requestBuckets.length ? requestBuckets : buckets;

  const modelId = (b: unknown) => String(obj(b)?.modelId ?? "").toLowerCase();
  const geminiPro = pool.filter((b) => modelId(b).includes("gemini") && modelId(b).includes("pro"));
  const geminiFlash = pool.filter((b) => modelId(b).includes("gemini") && modelId(b).includes("flash"));

  const session = mostUsed(geminiPro) ?? mostUsed(geminiFlash) ?? mostUsed(pool);
  if (session === null) return null;
  const weekly = mostUsed(geminiFlash) ?? mostUsed(geminiPro) ?? mostUsed(pool) ?? session;

  return {
    sessionPercent: Math.round(session),
    weeklyPercent: Math.round(weekly),
    resetsInSeconds: null,
  };
}
