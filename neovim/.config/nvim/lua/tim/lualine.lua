-- My config for lualine
--
-- Tim de Klijn, 2021

-- Init and configure lualine
require('lualine').setup{
  options = {
    theme = 'catppuccin',
    -- Unset separators, keep it simple
    section_separators = {'', ''},
    component_separators = {'', ''}
    -- section_separators = { left='', right='' },    
    -- component_separators = { left = '', right = ''}
  },
}


