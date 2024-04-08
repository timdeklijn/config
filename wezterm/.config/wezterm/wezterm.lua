local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "Berkeley Mono"
config.font_size = 28.0
config.line_height = 1.0

-- Wezterm settings
config.bell_volume = "0.0"
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 0,
  top= 0,
}

config.window_background_opacity = 1.0

-- Color settings
config.color_scheme = 'Molokai (Gogh)'

-- config.colors = {
--   background = "#151515"
-- }

return config
