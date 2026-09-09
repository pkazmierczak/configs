-- ~/.config/hypr/theme.lua
--
-- Solarized palette shared with waybar/mako/hyprlock, the Lua equivalent of
-- sway's `theme/light` and `theme/dark` palette files.
--
-- The active mode is read from ~/.cache/theme-mode, which is written by
-- scripts/theme-toggle.sh. Hyprland re-runs this file on every `hyprctl
-- reload`, so flipping the state file and reloading swaps the palette.

local palettes = {
    light = {
        name     = "light",
        bg       = "rgb(fdf6e3)",
        bg_alt   = "rgb(eee8d5)",
        fg       = "rgb(657b83)",
        accent   = "rgb(268bd2)",
        urgent   = "rgb(dc322f)",
        inactive = "rgb(93a1a1)",
    },
    dark = {
        name     = "dark",
        bg       = "rgb(002b36)",
        bg_alt   = "rgb(073642)",
        fg       = "rgb(839496)",
        accent   = "rgb(268bd2)",
        urgent   = "rgb(dc322f)",
        inactive = "rgb(586e75)",
    },
}

local mode = "light"

local state = io.open(os.getenv("HOME") .. "/.cache/theme-mode", "r")
if state then
    local line = state:read("*l")
    state:close()
    if line then
        line = line:gsub("%s+", "")
        if palettes[line] then
            mode = line
        end
    end
end

return palettes[mode]
