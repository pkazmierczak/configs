#!/usr/bin/env bash
# ~/.config/hypr/scripts/screenshot.sh
# Hyprland replacement for `grimshot savecopy anything`: saves the shot to
# ~/Pictures *and* puts it on the clipboard.
#
# grimshot's window/output selection talks to swaymsg, so the equivalent
# queries are done through hyprctl here.
#
# Usage: screenshot.sh [area|window|output]   (default: area)

set -euo pipefail

mode="${1:-area}"
dir="${XDG_PICTURES_DIR:-$HOME/Pictures}"
mkdir -p "$dir"
file="$dir/screenshot-$(date +%Y%m%d-%H%M%S).png"

case "$mode" in
    area)
        geometry=$(slurp) || exit 0
        grim -g "$geometry" "$file"
        ;;
    window)
        # Feed slurp the geometry of every visible window so it can snap to one.
        workspace=$(hyprctl activeworkspace -j | jq -r '.id')
        geometry=$(hyprctl clients -j \
            | jq -r --argjson ws "$workspace" \
                '.[] | select(.workspace.id == $ws and .hidden == false and .mapped == true)
                     | "\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"' \
            | slurp -r) || exit 0
        grim -g "$geometry" "$file"
        ;;
    output)
        output=$(hyprctl activeworkspace -j | jq -r '.monitor')
        grim -o "$output" "$file"
        ;;
    *)
        echo "Usage: $0 [area|window|output]" >&2
        exit 1
        ;;
esac

wl-copy --type image/png < "$file"
notify-send "Screenshot" "Saved to $file and copied to clipboard"
