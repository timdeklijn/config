-- My config for lualine
--
-- Tim de Klijn, 2021

-- Use build in LSP to get diagnostics and convert to string.
get_lsp_diagnostic = function()
  local result = {}
  local levels = {
    errors = 'Error',
    warnings = 'Warning',
    info = 'Information',
    hints = 'Hint'
  }

  for k, level in pairs(levels) do
    result[k] = vim.lsp.diagnostic.get_count(0, level)
  end

  return string.format(
    " :%s :%s :%s :%s ",
    result['errors'] or 0, result['warnings'] or 0,
    result['info'] or 0, result['hints'] or 0
  )
end

-- Init and configure lualine
require('lualine').setup{
  options = {
    theme = 'gruvbox_material',
    -- Unset separators, keep it simple
    section_separators = {'', ''},
    component_separators = {'', ''}
  },
  sections = {
    lualine_x = {get_lsp_diagnostic}
  }
}


