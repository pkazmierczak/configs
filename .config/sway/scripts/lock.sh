#!/usr/bin/env bash
set -euo pipefail

dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

args=()
for out in $(swaymsg -t get_outputs | jq -r '.[] | select(.active) | .name'); do
  grim -o "$out" "$dir/$out.png"
  magick "$dir/$out.png" -scale 5% -scale 2000% -font 'Source-Code-Pro' -gravity center -pointsize 100 -fill blue -annotate 0 'sod off' "$dir/$out.png"
  args+=(-i "$out:$dir/$out.png")
done

swaylock -f "${args[@]}"
