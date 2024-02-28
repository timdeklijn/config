local wezterm = require("wezterm")
local config = {}

-- font config
config.font = wezterm.font "JetBrainsMono Nerd Font"
config.font_size = 20.0
config.line_height = 1.3
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 0,
  top= 0,
}

-- Colors
config.color_scheme = "Catppuccin Frappe"

return config
