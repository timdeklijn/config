local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "0xProto Nerd Font Mono"
config.font_size = 30.0
config.line_height = 1.1

-- Wezterm settings
config.bell_volume = "0.0"
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 0,
  top= 0,
}

config.window_background_opacity = 0.8

-- Color settings
config.color_scheme = 'Gruvbox Dark (Gogh)'
config.colors = {
  -- Overwrite background color
  background = "black"
}

return config
