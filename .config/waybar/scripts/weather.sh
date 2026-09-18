#!/usr/bin/env bash
# ~/.config/waybar/scripts/weather.sh
# Prints JSON for waybar's custom/weather module.
# Location is auto-detected from IP (no API key, no account needed).

# The APIs always return numbers with a '.' decimal point (JSON spec).
# bash's printf parses %f arguments using the current LC_NUMERIC locale,
# so under e.g. pl_PL (comma decimal) it would reject "17.5" as invalid.
export LC_NUMERIC=C

fallback() {
    printf '{"text": "🌡️ n/a", "tooltip": "Weather unavailable"}\n'
    exit 0
}

GEO_RAW=$(curl -sS --max-time 8 "http://ip-api.com/json/?fields=lat,lon,city") || fallback
[ -n "$GEO_RAW" ] || fallback

LAT=$(echo "$GEO_RAW" | jq -r '.lat')
LON=$(echo "$GEO_RAW" | jq -r '.lon')
CITY=$(echo "$GEO_RAW" | jq -r '.city')
[ -n "$LAT" ] && [ "$LAT" != "null" ] || fallback

WEATHER=$(curl -sS --max-time 8 "https://api.open-meteo.com/v1/forecast?latitude=${LAT}&longitude=${LON}&current=temperature_2m,weather_code&timezone=auto") || fallback
[ -n "$WEATHER" ] || fallback

TEMP=$(echo "$WEATHER" | jq -r '.current.temperature_2m')
CODE=$(echo "$WEATHER" | jq -r '.current.weather_code')
[ -n "$TEMP" ] && [ "$TEMP" != "null" ] || fallback

case "$CODE" in
    0) ICON="☀️"; DESC="Clear sky" ;;
    1|2) ICON="🌤️"; DESC="Partly cloudy" ;;
    3) ICON="☁️"; DESC="Overcast" ;;
    45|48) ICON="🌫️"; DESC="Fog" ;;
    51|53|55) ICON="🌦️"; DESC="Drizzle" ;;
    61|63|65) ICON="🌧️"; DESC="Rain" ;;
    71|73|75) ICON="🌨️"; DESC="Snow" ;;
    80|81|82) ICON="🌧️"; DESC="Rain showers" ;;
    95|96|99) ICON="⛈️"; DESC="Thunderstorm" ;;
    *) ICON="🌡️"; DESC="Unknown" ;;
esac

printf '{"text": "%s %.0f°C", "tooltip": "%s — %s, %.0f°C"}\n' "$ICON" "$TEMP" "$CITY" "$DESC" "$TEMP"
