local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "JetBrainsMono NFP"
config.font_size = 16.0
config.line_height = 1.1

-- Wezterm settings
config.audible_bel = "Disabled"
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 0,
  top= 0,
}

-- Color settings
config.color_scheme = "nord"

return config
