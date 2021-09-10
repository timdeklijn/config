-- My config for telescope
--
-- Tim de Klijn, 2021

-- load themes
themes = require('telescope.themes')

-- Add fzf
require('telescope').setup {
  defaults = {
    generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
    file_sorter =  require'telescope.sorters'.get_fzy_sorter,
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = false,
      override_file_sorter = true,
      case_mode = "smart_case",
    }
  }
}

-- Telescope extensions
require('telescope').load_extension('fzf')

-- Add shortcuts:

-- Do not type this all the time:
local opts = { noremap=true, silent=true }

-- Search (open) buffers
vim.api.nvim_set_keymap('n', '<leader><space>', [[<cmd>lua require('telescope.builtin').buffers(themes.get_ivy({}))<cr>]],  opts)

-- Files --
vim.api.nvim_set_keymap('n', '<leader>f', [[<cmd>lua require('telescope.builtin').find_files(themes.get_ivy({}))<cr>]],  opts)
-- Look for previously opened files
vim.api.nvim_set_keymap('n', '<leader>s?', [[<cmd>lua require('telescope.builtin').oldfiles(themes.get_ivy({}))<cr>]],  opts)
-- File browser
vim.api.nvim_set_keymap('n', '<leader>sse', [[<cmd>lua require('telescope.builtin').file_browser(themes.get_ivy({}))<cr>]],  opts)


-- Treesitter variables in file
vim.api.nvim_set_keymap('n', '<leader>sl', [[<cmd>lua require('telescope.builtin').treesitter(themes.get_ivy({}))<cr>]],  opts)

-- Greps --
-- Find in current file:
vim.api.nvim_set_keymap('n', '<leader>ssf', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find(themes.get_ivy({}))<cr>]],  opts)
-- Live grep in project
vim.api.nvim_set_keymap('n', '<leader>ssg', [[<cmd>lua require('telescope.builtin').live_grep(themes.get_ivy({}))<cr>]],  opts)
-- Grep word under cursor
vim.api.nvim_set_keymap('n', '<leader>ssw', [[<cmd>lua require('telescope.builtin').grep_string(themes.get_ivy({}))<CR>]], opts)

-- Git shortcuts --
-- List all branches
vim.api.nvim_set_keymap('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_branches(themes.get_ivy({}))<cr>]],  opts)
-- List all commits
vim.api.nvim_set_keymap('n', '<leader>ga', [[<cmd>lua require('telescope.builtin').git_commits(themes.get_ivy({}))<cr>]],  opts)
-- List commits for current file
vim.api.nvim_set_keymap('n', '<leader>gc', [[<cmd>lua require('telescope.builtin').git_bcommits(themes.get_ivy({}))<cr>]],  opts)
