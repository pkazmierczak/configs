#!/usr/bin/env bash
# ~/.config/hypr/scripts/theme-toggle.sh
# Hyprland version of ~/.config/sway/scripts/theme-toggle.sh.
#
# Switches Hyprland, waybar, mako and GTK/libadwaita apps between Solarized
# Light and Solarized Dark in one shot.
#
# Unlike the sway version there is no palette file to copy for the compositor:
# ~/.config/hypr/theme.lua reads the mode out of the state file below, so
# writing the state file and reloading is enough.
#
# Usage: theme-toggle.sh          (flips whichever mode is active)
#        theme-toggle.sh light    (force light)
#        theme-toggle.sh dark     (force dark)

set -euo pipefail

WAYBAR_DIR="$HOME/.config/waybar"
MAKO_DIR="$HOME/.config/mako"
STATE_FILE="$HOME/.cache/theme-mode"

mkdir -p "$HOME/.cache"

current="light"
[ -f "$STATE_FILE" ] && current=$(cat "$STATE_FILE")

target="${1:-}"
if [ -z "$target" ]; then
    if [ "$current" = "light" ]; then target="dark"; else target="light"; fi
fi

if [ "$target" != "light" ] && [ "$target" != "dark" ]; then
    echo "Usage: $0 [light|dark]" >&2
    exit 1
fi

# Write the state first: theme.lua reads it while Hyprland reloads.
echo "$target" > "$STATE_FILE"

# hyprland — theme.lua picks the palette back up on reload
hyprctl reload

# waybar — needs "reload_style_on_change": true in the config to pick this up
# live; falls back to a restart if that's not set.
cp "$WAYBAR_DIR/style-$target.css" "$WAYBAR_DIR/style.css"

# mako
cp "$MAKO_DIR/config-$target" "$MAKO_DIR/config"
makoctl reload

# GTK / libadwaita apps, best-effort — harmless no-op if gsettings/the
# schema isn't available
if command -v gsettings >/dev/null 2>&1; then
    scheme="default"
    [ "$target" = "dark" ] && scheme="prefer-dark"
    gsettings set org.gnome.desktop.interface color-scheme "$scheme" 2>/dev/null || true
fi

notify-send "Theme" "Switched to $target mode"
