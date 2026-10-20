#!/usr/bin/env bash
# ~/.config/sway/scripts/power-profile.sh
# Switch the power profile (power-saver / balanced / performance).
#
# Usage: power-profile.sh              (pick from a fuzzel menu)
#        power-profile.sh performance  (set directly)
#
# Talks to the daemon over D-Bus with busctl (part of systemd) rather than
# powerprofilesctl, because Fedora 41+ uses tuned-ppd, which implements the
# D-Bus API but ships no CLI. Works unchanged with power-profiles-daemon too.
# Waybar's power-profiles-daemon module watches the same property, so the bar
# follows along on its own.

set -euo pipefail

bus=(org.freedesktop.UPower.PowerProfiles
     /org/freedesktop/UPower/PowerProfiles
     org.freedesktop.UPower.PowerProfiles)

# busctl prints a string property as: s "balanced"
if ! current=$(busctl get-property "${bus[@]}" ActiveProfile 2>/dev/null); then
    notify-send -u critical "Power profile" \
        "No power profile daemon on D-Bus.\nsudo dnf install tuned-ppd"
    exit 1
fi
current=$(cut -d'"' -f2 <<<"$current")

target="${1:-}"
if [ -z "$target" ]; then
    # Profiles is aa{sv}, printed as:
    #   aa{sv} 3 3 "Profile" s "power-saver" "CpuDriver" s "intel_pstate" ...
    # Only offer what this machine supports (some have no "performance").
    target=$(busctl get-property "${bus[@]}" Profiles \
        | grep -o '"Profile" s "[^"]*"' \
        | cut -d'"' -f4 \
        | fuzzel --dmenu --prompt "Power ($current): ") || exit 0
fi

if [ -z "$target" ] || [ "$target" = "$current" ]; then
    exit 0
fi

if ! err=$(busctl set-property "${bus[@]}" ActiveProfile s "$target" 2>&1); then
    notify-send -u critical "Power profile" "Could not switch to $target: $err"
    exit 1
fi
notify-send "Power profile" "$current → $target"
