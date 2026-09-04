#!/usr/bin/env bash
# ~/.config/waybar/scripts/network-menu.sh
# Pick a Wi-Fi network via fuzzel and connect to it (nmcli-backed).

set -euo pipefail

mapfile -t networks < <(nmcli -t -f SSID,SECURITY,SIGNAL device wifi list --rescan yes \
    | awk -F: '!seen[$1]++ && $1 != "" {printf "%-30s %s  %s%%\n", $1, ($2=="--"?"open":"secured"), $3}')

choice=$(printf '%s\n' "${networks[@]}" | fuzzel --dmenu --prompt "Wi-Fi: ")
[ -z "$choice" ] && exit 0

ssid=$(echo "$choice" | awk '{print $1}')

# Already have a saved connection profile for this SSID? Just bring it up.
if nmcli -t -f NAME connection show | grep -qx "$ssid"; then
    nmcli connection up "$ssid"
else
    security=$(echo "$choice" | awk '{print $2}')
    if [ "$security" = "open" ]; then
        nmcli device wifi connect "$ssid"
    else
        password=$(fuzzel --dmenu --password --prompt "Password for $ssid: ")
        [ -z "$password" ] && exit 0
        nmcli device wifi connect "$ssid" password "$password"
    fi
fi

notify-send "Wi-Fi" "Connecting to $ssid…"
