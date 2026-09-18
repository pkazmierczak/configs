# Hyprland config

A port of `~/.config/sway/config` (and its waybar/mako companions) to Hyprland.
The sway config is left untouched, so both sessions can coexist.

## Layout

| File | Replaces |
| --- | --- |
| `hyprland.lua` | `~/.config/sway/config` |
| `theme.lua` | `~/.config/sway/theme/{light,dark}` |
| `hyprpaper.conf` | `output * bg ~/wallpaper.jpg fill` |
| `hypridle.conf` | the `swayidle` block |
| `hyprlock.conf` | `sway/scripts/lock.sh` + `swaylock` |
| `scripts/theme-toggle.sh` | `sway/scripts/theme-toggle.sh` |
| `scripts/screenshot.sh` | `grimshot savecopy anything` |
| `../waybar/config-hyprland.jsonc` | `../waybar/config.jsonc` |
| `../mako/config-{light,dark}` | unchanged — mako works as-is |

> **Config format:** Hyprland ≥ 0.55 uses a Lua config (`hyprland.lua`); the old
> `hyprland.conf` hyprlang format is deprecated (0.56.1 added a deprecation
> notice for it). Everything here is written against the Lua API.

## Dependencies

Same as the sway setup, minus `sway`/`swayidle`/`swaylock`/`swaybg`/`grimshot`, plus:

```
hyprland hyprpaper hypridle hyprlock
waybar mako fuzzel ghostty cliphist wl-clipboard
grim slurp jq wireplumber brightnessctl
```

`hyprshutdown` is used for the exit bind if present, with `hl.dsp.exit()` as fallback.

## Binding map

Everything below is unchanged from the sway config unless noted.

| Bind | sway | Hyprland |
| --- | --- | --- |
| `$mod+Return` | `exec $term` | `hl.dsp.exec_cmd(term)` |
| `$mod+Shift+q` | `kill` | `window.close()` |
| `$mod+d` | `exec fuzzel` | same |
| `$mod+Shift+e` | `swaynag` → `swaymsg exit` | `hyprshutdown` ‖ `hl.dsp.exit()` |
| `$mod+Escape` | `lock.sh` | `loginctl lock-session` → hyprlock |
| `$mod+Shift+c` | `reload` | `reload_config()` |
| `$mod+hjkl` / arrows | `focus <dir>` | `focus({ direction })` |
| `$mod+Shift+hjkl` | `move <dir>` | `window.move({ direction })` |
| `$mod+b` / `$mod+v` | `splith` / `splitv` | `layout("preselect r"/"preselect d")` |
| `$mod+e` | `layout toggle split` | `layout("togglesplit")` |
| `$mod+f` | `fullscreen` | `window.fullscreen()` |
| `$mod+Shift+space` | `floating toggle` | `window.float({ action = "toggle" })` |
| `$mod+1..0` | `workspace number N` | `focus({ workspace = N })` |
| `$mod+Shift+1..0` | `move container to workspace N` | `window.move({ workspace = N, follow = false })` |
| `$mod+r` | `mode "resize"` | `submap("resize")` |
| `Print` | `grimshot savecopy anything` | `scripts/screenshot.sh area` |

### Deliberate differences

* **`$mod+w` / `$mod+s`** — Hyprland has no `tabbed`/`stacking` layouts. The
  closest equivalent is groups, so `$mod+w` toggles a group (which renders as a
  tab bar) and `$mod+s` cycles to the next window inside it.
* **`$mod+a`** — dwindle has no `focus parent`. The key is reused for
  `move out of group`, the counterpart to `$mod+w`.
* **`$mod+space`** — `focus mode_toggle` doesn't exist; the bind is a small Lua
  function that cycles into the floating stack when a tiled window is focused
  and vice versa.
* **Gaps** — sway's `gaps inner 6` is the gap *between* windows; Hyprland's
  `gaps_in` is applied per window edge, hence `gaps_in = 3`.
* **`smart_borders`** — no equivalent. It would need a window rule setting
  `border_size = 0` for the single-window case (see the "smart gaps" recipe in
  the upstream example config).
* **`client.urgent`** — Hyprland has no urgent border colour, so `theme.urgent`
  is currently unused by the compositor (waybar/mako still use it).
* **Screenshots** — `grimshot` shells out to `swaymsg`, so window/output
  selection is reimplemented on top of `hyprctl` in `scripts/screenshot.sh`.
* **Lock screen** — `lock.sh` pixelated a `grim` capture with ImageMagick.
  hyprlock takes its own screenshot (`path = screenshot`) but has no pixelate
  filter, so `hyprlock.conf` blurs instead. The "sod off" label survives.

### Theme toggling

`$mod+Shift+t` runs `scripts/theme-toggle.sh`, which writes `light`/`dark` to
`~/.cache/theme-mode` and then reloads. `theme.lua` reads that file on every
config load, so no palette file is copied for the compositor (waybar CSS and
mako configs are still copied into place exactly like the sway version).

Force a mode with `theme-toggle.sh light` / `theme-toggle.sh dark`.

## Window rules

sway matched Wayland apps on `app_id`; Hyprland calls the same property
`class`. The rules in `hyprland.lua` reuse the sway strings verbatim and anchor
them (`^…$`). If a rule doesn't fire, check the real value with:

```sh
hyprctl clients
```

XWayland apps in particular may report a different class than their sway
`app_id`.
