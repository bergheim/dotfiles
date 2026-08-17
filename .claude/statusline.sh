#!/bin/bash
# Claude Code status line. Receives session JSON on stdin, prints one line.
input=$(cat)

eval "$(echo "$input" | jq -r '
  @sh "MODEL=\(.model.display_name)
  DIR=\(.workspace.current_dir)
  PCT=\(.context_window.used_percentage // 0 | floor)
  IN_TOK=\(.context_window.total_input_tokens // 0)
  CTX_SIZE=\(.context_window.context_window_size // 200000)
  COST=\(.cost.total_cost_usd // 0)
  DURATION_MS=\(.cost.total_duration_ms // 0)
  ADDED=\(.cost.total_lines_added // 0)
  REMOVED=\(.cost.total_lines_removed // 0)
  EFFORT=\(.effort.level // "")
  FIVE_H=\(.rate_limits.five_hour.used_percentage // "" )
  FIVE_H_RESET=\(.rate_limits.five_hour.resets_at // "")
  WEEK=\(.rate_limits.seven_day.used_percentage // "")
  WEEK_RESET=\(.rate_limits.seven_day.resets_at // "")"')"

CYAN=$'\033[36m'; GREEN=$'\033[32m'; YELLOW=$'\033[33m'; RED=$'\033[31m'
# Mid-gray (xterm 245, #8a8a8a) rather than bright-black 90: 90 renders too
# close to the background in dark themes, and 245 stays legible in both.
DIM=$'\033[38;5;245m'; RESET=$'\033[0m'

# Context bar, colored by usage
if [ "$PCT" -ge 90 ]; then BAR_COLOR="$RED"
elif [ "$PCT" -ge 70 ]; then BAR_COLOR="$YELLOW"
else BAR_COLOR="$GREEN"; fi
FILLED=$((PCT / 10)); EMPTY=$((10 - FILLED))
printf -v FILL "%${FILLED}s"; printf -v PAD "%${EMPTY}s"
BAR="${FILL// /█}${PAD// /░}"

# Tokens as 62k/200k
fmt_k() { echo $((${1%%.*} / 1000))k; }
TOKENS="$(fmt_k "$IN_TOK")/$(fmt_k "$CTX_SIZE")"

# Git branch (cheap: no status/diff)
BRANCH=$(git -C "$DIR" branch --show-current 2>/dev/null)

MINS=$((DURATION_MS / 60000))
COST_FMT=$(printf '$%.2f' "$COST")

LINE="${CYAN}[$MODEL${EFFORT:+ $EFFORT}]${RESET}"
LINE+=" ${DIR##*/}${BRANCH:+ ${DIM}${RESET}$BRANCH}"
LINE+=" ${BAR_COLOR}${BAR}${RESET} ${PCT}% ${DIM}(${TOKENS})${RESET}"
LINE+=" ${YELLOW}${COST_FMT}${RESET} ${DIM}${MINS}m +${ADDED}/-${REMOVED}${RESET}"

# Color a limit percentage by how close it is: yellow >=70, red >=90
limit_pct() {
    local p=${1%%.*}
    if [ "$p" -ge 90 ]; then echo "${RED}${p}%${RESET}${DIM}"
    elif [ "$p" -ge 70 ]; then echo "${YELLOW}${p}%${RESET}${DIM}"
    else echo "${p}%"; fi
}

LIMITS=""
if [ -n "$FIVE_H" ]; then
    LIMITS="5h $(limit_pct "$FIVE_H")"
    [ -n "$FIVE_H_RESET" ] && LIMITS+="→$(date -d "@${FIVE_H_RESET%%.*}" +%H:%M)"
fi
if [ -n "$WEEK" ]; then
    LIMITS="${LIMITS:+$LIMITS }7d $(limit_pct "$WEEK")"
    [ -n "$WEEK_RESET" ] && LIMITS+="→$(date -d "@${WEEK_RESET%%.*}" +%a\ %H:%M)"
fi
[ -n "$LIMITS" ] && LINE+=" ${DIM}${LIMITS}${RESET}"

printf '%s\n' "$LINE"
