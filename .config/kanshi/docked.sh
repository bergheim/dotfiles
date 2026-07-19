#!/bin/sh
# Runs from the kanshi "docked" profile after the external is configured.
#
# Workspace relocation is handled by niri itself: workspaces 1-6 are pinned to
# the external via `open-on-output "DP-2"` in ~/.config/niri/config.kdl, so niri
# moves them (and their windows) onto it on connect. This script just focuses
# the external so it becomes the active monitor (new/dynamic windows land there).
#
# Brief wait so niri finishes relocating the pinned workspaces before we focus.
sleep 0.4
niri msg action focus-monitor "DP-2"
