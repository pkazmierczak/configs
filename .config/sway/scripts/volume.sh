#!/usr/bin/env bash
# ~/.config/sway/scripts/volume.sh
# Usage: volume.sh up|down|mute
# Changes volume via wpctl, then shows a mako notification with a progress bar.
# The synchronous hint makes each press replace the previous notification
# instead of stacking a new one.

export LC_ALL=C   # keep wpctl/awk number handling locale-independent

SINK=@DEFAULT_AUDIO_SINK@

case "$1" in
    up)   wpctl set-volume "$SINK" 5%+ ;;
    down) wpctl set-volume "$SINK" 5%- ;;
    mute) wpctl set-mute "$SINK" toggle ;;
    *)    echo "Usage: $0 up|down|mute" >&2; exit 1 ;;
esac

out=$(wpctl get-volume "$SINK")
vol=$(awk '{printf "%d", $2 * 100 + 0.5}' <<< "$out")
bar=$(( vol > 100 ? 100 : vol ))

if grep -q MUTED <<< "$out"; then
    text="Muted"
    bar=0
else
    text="${vol}%"
fi

notify-send -t 1500 \
    -h string:x-canonical-private-synchronous:volume \
    -h int:value:"$bar" \
    "Volume" "$text"
