local wezterm = require("wezterm")
local config = {}

-- shell to start in
config.default_prog = { "bash" }

-- font config
config.font = wezterm.font "ComicShannsMono Nerd Font"
config.font_size = 25.0
config.line_height = 1.25

-- Simple tabs: looks more terminal like
config.use_fancy_tab_bar = false

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
