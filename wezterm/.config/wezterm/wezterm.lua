local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "CommitMono Nerd Font"
config.font_size = 21.0
config.line_height = 1.2

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
