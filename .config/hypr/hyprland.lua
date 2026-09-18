-- ~/.config/hypr/hyprland.lua
--
-- Hyprland port of ~/.config/sway/config.
-- Requires Hyprland >= 0.55 (Lua config; `.conf` configs are deprecated).
-- See ./README.md for the full sway -> Hyprland mapping and the handful of
-- bindings that have no exact equivalent.

local theme = require("theme")

local hypr_dir = os.getenv("HOME") .. "/.config/hypr"

------------------------------------------------------------------- variables

local mod  = "SUPER"
local term = "ghostty"
local menu = "fuzzel"

---------------------------------------------------------------------- output

-- sway: `output * bg ~/wallpaper.jpg fill`
-- The wallpaper itself is handled by hyprpaper (see hyprpaper.conf); Hyprland
-- only owns the mode/position/scale of the outputs.
hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = "auto",
})

----------------------------------------------------------------------- input

hl.config({
    input = {
        kb_layout  = "pl",
        kb_options = "ctrl:nocaps",

        touchpad = {
            tap_to_click         = true,
            natural_scroll       = true,
            disable_while_typing = true,
        },
    },
})

------------------------------------------------------------------ appearance

hl.config({
    general = {
        -- sway's `gaps inner 6` is the gap *between* two windows, while
        -- Hyprland's gaps_in is applied to each window edge -> half of it.
        gaps_in  = 3,
        gaps_out = 0,

        border_size = 2,
        layout      = "dwindle",

        col = {
            active_border   = theme.accent,
            inactive_border = theme.bg_alt,
        },
    },

    -- Keep the flat, sway-like look: no rounding, blur or shadows.
    decoration = {
        rounding = 0,
        blur   = { enabled = false },
        shadow = { enabled = false },
    },

    dwindle = {
        -- Required for `togglesplit` ($mod+E) to do anything, since dwindle
        -- otherwise picks the split direction from the window's aspect ratio.
        preserve_split = true,
    },

    misc = {
        disable_hyprland_logo   = true,
        force_default_wallpaper = 0,
    },
})

------------------------------------------------------------------- autostart

hl.on("hyprland.start", function()
    hl.exec_cmd("waybar -c " .. os.getenv("HOME") .. "/.config/waybar/config-hyprland.jsonc")
    hl.exec_cmd("mako")
    hl.exec_cmd("hyprpaper")
    hl.exec_cmd("hypridle") -- replaces swayidle; see hypridle.conf

    hl.exec_cmd("wl-paste --type text --watch cliphist store")
    hl.exec_cmd("wl-paste --type image --watch cliphist store")

    -- Import GTK/env vars into the systemd/dbus user session so the waybar
    -- tray, polkit agents, etc. behave. Harmless if these don't exist.
    hl.exec_cmd("dbus-update-activation-environment --systemd --all")
    hl.exec_cmd("systemctl --user import-environment DISPLAY WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP")

    -- Polkit agent (needed for GUI password prompts). `hyprpolkitagent` is the
    -- Hyprland-native alternative if you'd rather not pull in polkit-gnome.
    hl.exec_cmd("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1")

    -- sway: `exec swaymsg "workspace 1; exec ..."`. The rules table pins the
    -- spawned window to workspace 1; `silent` keeps focus where it is.
    hl.dispatch(hl.dsp.exec_cmd(os.getenv("HOME") .. "/appImages/beeper.AppImage", { workspace = "1 silent" }))
    hl.dispatch(hl.dsp.exec_cmd("1password --silent", { workspace = "1 silent" }))
    hl.dispatch(hl.dsp.exec_cmd("kopia-ui", { workspace = "1 silent" }))
end)

--------------------------------------------------------------- core bindings

hl.bind(mod .. " + Return",    hl.dsp.exec_cmd(term), { description = "Terminal" })
hl.bind(mod .. " + SHIFT + Q", hl.dsp.window.close(), { description = "Close window" })
hl.bind(mod .. " + D",         hl.dsp.exec_cmd(menu), { description = "App launcher" })

-- Exit (swaynag -> hyprshutdown, with a plain exit as fallback)
hl.bind(mod .. " + SHIFT + E",
    hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'"),
    { description = "Exit Hyprland" })

-- Lock screen. Going through logind means hypridle, the lid switch and this
-- bind all end up in the same hyprlock instance.
hl.bind(mod .. " + Escape", hl.dsp.exec_cmd("loginctl lock-session"), { description = "Lock" })

-- Clipboard history picker
hl.bind(mod .. " + SHIFT + V",
    hl.dsp.exec_cmd("cliphist list | fuzzel --dmenu | cliphist decode | wl-copy"),
    { description = "Clipboard history" })

-- Toggle light/dark theme
hl.bind(mod .. " + SHIFT + T",
    hl.dsp.exec_cmd(hypr_dir .. "/scripts/theme-toggle.sh"),
    { description = "Toggle light/dark theme" })

-- Reload config
hl.bind(mod .. " + SHIFT + C", hl.dsp.reload_config(), { description = "Reload config" })

-- Screenshot (grimshot savecopy anything -> scripts/screenshot.sh)
hl.bind("Print",         hl.dsp.exec_cmd(hypr_dir .. "/scripts/screenshot.sh area"))
hl.bind("SHIFT + Print", hl.dsp.exec_cmd(hypr_dir .. "/scripts/screenshot.sh window"))
hl.bind(mod .. " + Print", hl.dsp.exec_cmd(hypr_dir .. "/scripts/screenshot.sh output"))

--------------------------------------------------------------- focus / move

local motions = {
    { key = "H",     dir = "left"  },
    { key = "J",     dir = "down"  },
    { key = "K",     dir = "up"    },
    { key = "L",     dir = "right" },
    { key = "left",  dir = "left"  },
    { key = "down",  dir = "down"  },
    { key = "up",    dir = "up"    },
    { key = "right", dir = "right" },
}

for _, m in ipairs(motions) do
    hl.bind(mod .. " + " .. m.key,             hl.dsp.focus({ direction = m.dir }))
    hl.bind(mod .. " + SHIFT + " .. m.key,     hl.dsp.window.move({ direction = m.dir }))
end

--------------------------------------------------------------------- layout

-- sway `splith` / `splitv`: dwindle's preselect is a one-shot override for
-- where the *next* window lands.
hl.bind(mod .. " + B", hl.dsp.layout("preselect r"), { description = "Split horizontally" })
hl.bind(mod .. " + V", hl.dsp.layout("preselect d"), { description = "Split vertically" })
hl.bind(mod .. " + E", hl.dsp.layout("togglesplit"), { description = "Toggle split direction" })

-- sway `layout tabbed` / `layout stacking`: Hyprland's groups are the closest
-- thing (one slot, tab bar, cycle through members).
hl.bind(mod .. " + W", hl.dsp.group.toggle(), { description = "Toggle group (tabbed)" })
hl.bind(mod .. " + S", hl.dsp.group.next(),   { description = "Next window in group" })
-- sway's `focus parent` has no dwindle equivalent; $mod+A pops a window back
-- out of its group instead, as the counterpart to $mod+W.
hl.bind(mod .. " + A", hl.dsp.window.move({ out_of_group = true }), { description = "Leave group" })

hl.bind(mod .. " + F",         hl.dsp.window.fullscreen())
hl.bind(mod .. " + SHIFT + space", hl.dsp.window.float({ action = "toggle" }))

-- sway `focus mode_toggle`: jump between the tiled and the floating stack.
hl.bind(mod .. " + space", function()
    local w = hl.get_active_window()
    if w ~= nil and w.floating then
        hl.dispatch(hl.dsp.window.cycle_next({ tiled = true }))
    else
        hl.dispatch(hl.dsp.window.cycle_next({ floating = true }))
    end
end, { description = "Focus tiled/floating" })

----------------------------------------------------------------- workspaces

for i = 1, 10 do
    local key = i % 10 -- workspace 10 lives on the 0 key

    hl.bind(mod .. " + " .. key, hl.dsp.focus({ workspace = i }))
    -- follow = false matches sway: move the window, stay where you are.
    hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i, follow = false }))
end

---------------------------------------------------------------- resize mode

hl.bind(mod .. " + R", hl.dsp.submap("resize"), { description = "Resize mode" })

hl.define_submap("resize", function()
    local steps = {
        { keys = { "H", "left"  }, x = -10, y =   0 }, -- shrink width
        { keys = { "J", "down"  }, x =   0, y =  10 }, -- grow height
        { keys = { "K", "up"    }, x =   0, y = -10 }, -- shrink height
        { keys = { "L", "right" }, x =  10, y =   0 }, -- grow width
    }

    for _, step in ipairs(steps) do
        for _, key in ipairs(step.keys) do
            hl.bind(key, hl.dsp.window.resize({ x = step.x, y = step.y, relative = true }), { repeating = true })
        end
    end

    hl.bind("Return", hl.dsp.submap("reset"))
    hl.bind("Escape", hl.dsp.submap("reset"))
end)

--------------------------------------------------- media / volume / brightness

hl.bind("XF86AudioRaiseVolume",  hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"),  { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",  hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),  { locked = true, repeating = true })
hl.bind("XF86AudioMute",         hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl set +5%"),                      { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 5%-"),                      { locked = true, repeating = true })

--------------------------------------------------------------- window rules

-- sway matched Wayland apps on `app_id`; Hyprland calls the same thing
-- `class`. Check the real value with `hyprctl clients` if a rule misses.
local float_classes = {
    "Beeper",
    "org.gnome.Nautilus",
    "1password",
    "chrome-hnpfjngllnobngcgfapefoaidbinmjnm-Default", -- whatsapp webapp
    "org.pulseaudio.pavucontrol",
    "org.gnome.Software",
}

for _, class in ipairs(float_classes) do
    hl.window_rule({
        name  = "float-" .. class,
        match = { class = "^" .. class .. "$" },
        float = true,
    })
end
