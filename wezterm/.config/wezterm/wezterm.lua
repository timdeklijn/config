local wezterm = require("wezterm")
local config = {}

-- shell to start is based on the os we are running
if wezterm.target_triple == 'x86_64-apple-darwin' then
  -- load fish from brew install
  config.default_prog = { "/usr/local/bin/fish" }
else
  -- TODO: fish in linux?
  config.default_prog = { "bash" }
end


-- font config
config.font = wezterm.font "CaskaydiaCove Nerd Font"
config.font_size = 25.0
config.line_height = 1.25

-- Simple tabs: looks more terminal like
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

config.window_padding = {
  bottom = 2,
  top= 2,
}

-- Colors
config.color_scheme = 'nord'
config.colors = {
  tab_bar = {
    background = '#2e3440',
    active_tab = {
      bg_color = '#5e81ac',
      fg_color = '#eceff4',
    },
    inactive_tab = {
      bg_color = '#4c566a',
      fg_color = '#eceff4',
    },
    inactive_tab_hover = {
      bg_color = '#81a1c1',
      fg_color = '#eceff4',
    },
    new_tab = {
      bg_color = '#a3be8c',
      fg_color = '#eceff4',
    },
    new_tab_hover = {
      bg_color = '#81a1c1',
      fg_color = '#eceff4',
    },
  },
}

-- Define a leader key (ctrl+a)
config.leader = {
  key = 'a',
  mods = 'CTRL',
  timeout_milliseconds = 1000
}

-- Define shortcuts
config.keys = {
  -- ==========================================================================
  -- MISC
  -- ==========================================================================
  -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
  {
    key = 'a',
    mods = 'LEADER|CTRL',
    action = wezterm.action.SendKey { key = 'a', mods = 'CTRL' },
  },
  -- Enter copy mode
  {
    key = 'x',
    mods = 'LEADER|CTRL',
    action = wezterm.action.ActivateCopyMode
  },
  -- Quickselect: copy using shortcuts
  {
    key = 's',
    mods = 'LEADER|CTRL',
    action = wezterm.action.QuickSelect
  },
  -- ==========================================================================
  -- PANES
  -- ==========================================================================
  -- Horizontal pane split
  {
    key = '|',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  -- Vertival pane split
  {
    key = '_',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  -- Rotate frames clockwise
  {
    key = 'r',
    mods = 'LEADER',
    action = wezterm.action.RotatePanes 'Clockwise'
  },
  {
    key = 'r',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.RotatePanes 'CounterClockwise'
  },
  -- Focus on tab to the left
  {
    key = 'h',
    mods = 'LEADER',
    action = wezterm.action.ActivatePaneDirection 'Left',
  },
  -- Focus on tab to the right
  {
    key = 'l',
    mods = 'LEADER',
    action = wezterm.action.ActivatePaneDirection 'Right',
  },
  -- Focus on tab above
  {
    key = 'k',
    mods = 'LEADER',
    action = wezterm.action.ActivatePaneDirection 'Up',
  },
  -- Focus on tab to the bottom
  {
    key = 'j',
    mods = 'LEADER',
    action = wezterm.action.ActivatePaneDirection 'Down',
  },
  -- Make wezterm go full screen
  {
    key = 'f',
    mods = 'LEADER|CTRL',
    action = wezterm.action.ToggleFullScreen,
  },
  {
    key = 'H',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize { 'Left', 10 },
  },
  {
    key = 'J',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize { 'Down', 10 },
  },
  {
    key = 'K',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize { 'Up', 10 }
  },
  {
    key = 'L',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize { 'Right', 10 },
  },
  -- ==========================================================================
  -- TABS
  -- ==========================================================================
  -- Create tab
  {
    key = 't',
    mods = 'LEADER|CTRL',
    action = wezterm.action.SpawnTab 'CurrentPaneDomain',
  },
  -- previous tab
  {
    key = 'i',
    mods = 'LEADER',
    action = wezterm.action.ActivateTabRelative(-1)
  },
  -- next tab
  {
    key = 'o',
    mods = 'LEADER',
    action = wezterm.action.ActivateTabRelative(1)
  },
  -- close tab
  {
    key = 'w',
    mods = 'LEADER|CTRL',
    action = wezterm.action.CloseCurrentTab { confirm = true },
  },
}

return config
