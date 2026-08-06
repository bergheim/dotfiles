export function renderBar(usedPercent: number, width = 10): string {
  width = Math.max(0, width); // negative layout width would throw in repeat()
  const clamped = Number.isFinite(usedPercent) ? Math.max(0, Math.min(100, usedPercent)) : 0;
  const filled = Math.round((clamped / 100) * width);
  return "█".repeat(filled) + "░".repeat(width - filled);
}

export function barColor(usedPercent: number): "green" | "yellow" | "red" {
  if (usedPercent >= 90) return "red";
  if (usedPercent >= 70) return "yellow";
  return "green";
}

export function formatDuration(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds < 0) return "0s"; // untrusted API value: clamp to "resets now" instead of leaking NaN/Infinity/negatives
  if (seconds < 60) return `${Math.floor(seconds)}s`;
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`;
  return `${Math.floor(seconds / 3600)}h`;
}
