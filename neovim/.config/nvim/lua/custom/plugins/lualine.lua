-- Nice status line. Should give enought information but not be too cluttered.

return {
  'nvim-lualine/lualine.nvim',
  opts = {
    options = {
      globalstatus = true,
      icons_enabled = true,
      theme = 'tokyonight',
      component_separators = { left = '', right = ''},
      section_separators = { left = '', right = ''},
    },
  },
}

