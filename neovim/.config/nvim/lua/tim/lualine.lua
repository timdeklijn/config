-- My config for lualine
--
-- Tim de Klijn, 2021

-- Init and configure lualine
require('lualine').setup{
  options = {
    theme = 'OceanicNext',
    -- Unset separators, keep it simple
    section_separators = {'', ''},
    component_separators = {'', ''}
  },
}


