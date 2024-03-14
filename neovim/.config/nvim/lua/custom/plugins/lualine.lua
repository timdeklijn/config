-- Nice status line. Should give enought information but not be too cluttered.

return {
  'nvim-lualine/lualine.nvim',
  opts = {
    options = {
      icons_enabled = true,
      component_separators = '|',
      section_separators = '',
    },
  },
}

