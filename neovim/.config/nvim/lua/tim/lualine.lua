-- My config for lualine
--
-- Tim de Klijn, 2021

-- Init and configure lualine
require('lualine').setup{
  options = {
    -- theme = LUALINE_THEME,
    -- theme = 'tokyonight',
    theme = 'gruvbox-material',
    section_separators = {'', ''},
    component_separators = {'', ''},
    section_separators = { left = '', right = '' },
  },
}


