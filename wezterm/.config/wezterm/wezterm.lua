local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "Berkeley Mono"
config.font_size = 20.0
config.line_height = 1.0

-- Wezterm settings
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 10,
  top= 10,
}

config.window_background_opacity = 1.0

-- Color settings
config.color_scheme = 'Tokyo Night'

return config
