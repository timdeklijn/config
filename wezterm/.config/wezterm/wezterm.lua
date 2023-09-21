local wezterm = require("wezterm")
local config = {}

-- shell to start is based on the os we are running
if wezterm.target_triple == 'x86_64-apple-darwin' then
  config.default_prog = { "/usr/local/bin/fish" }
else
  config.default_prog = { "/usr/bin/fish" }
end

-- font config
config.font = wezterm.font "ComicShannsMono Nerd Font"
config.font_size = 25.0
config.line_height = 1.5
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  bottom = 1,
  top= 1,
}

-- -- Colors
config.color_scheme = 'Kanagawa (Gogh)'

return config
