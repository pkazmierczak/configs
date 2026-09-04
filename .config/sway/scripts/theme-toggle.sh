#!/usr/bin/env bash
# ~/.local/bin/theme-toggle.sh
# Switches sway, waybar, mako, and GTK/libadwaita apps between Solarized
# Light and Solarized Dark in one shot.
#
# Usage: theme-toggle.sh          (flips whichever mode is active)
#        theme-toggle.sh light    (force light)
#        theme-toggle.sh dark     (force dark)

set -euo pipefail

SWAY_THEME_DIR="$HOME/.config/sway/theme"
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

# sway — swap the included palette file and reload
cp "$SWAY_THEME_DIR/$target" "$SWAY_THEME_DIR/current"
swaymsg reload

# waybar — needs "reload_style_on_change": true in config.jsonc to pick
# this up live; falls back to a restart if that's not set.
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

echo "$target" > "$STATE_FILE"
notify-send "Theme" "Switched to $target mode"
