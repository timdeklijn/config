require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    }
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')

-- these are the same for each keymap
opts = {noremap = true, silent = true}

-- search in my dotfiles directory. This function is useful because it allows
-- to specify for this particular case to show hidden files (ignoring the git
-- folder).
my_config_files = function() 
  local config_dir = "/Users/timdeklijn/dotfiles"
  require("telescope.builtin").find_files({
      find_command = { 'rg', '--files', '--iglob', '!.git', '--hidden', config_dir },
    })
end

-- All finders are wrapped in functions for better highlighting and ease of
-- use.

-- Search all project files
my_find_files = function() 
  require("telescope.builtin").find_files()
end

-- Search through all open buffers
my_buffers = function()
  require("telescope.builtin").buffers({
    sort_lastused = true
  })
end

-- Live grep the project
my_live_grep = function()
  require("telescope.builtin").live_grep()
end

-- All references of word under cursor
my_lsp_references = function()
  require("telescope.builtin").lsp_references()
end

-- Live grep workspace symbols
my_lsp_workspace_symbols = function()
  require("telescope.builtin").lsp_dynamic_workspace_symbols()
end

-- fuzzy find in current buffer
my_current_buffer_fuzzy_find = function()
  require("telescope.builtin").current_buffer_fuzzy_find()
end

-- continue previous query in telescope
my_resume = function()
  require("telescope.builtin").resume()
end

-- List of keymaps
vim.api.nvim_set_keymap('n', '<leader>f', [[<cmd>lua my_find_files()<cr>]], opts)
vim.api.nvim_set_keymap('n', '<Leader><Space>', '<cmd>lua my_buffers()<cr>', opts)
vim.api.nvim_set_keymap('n', '<Leader>sg', '<cmd>lua my_live_grep()<cr>', opts)
vim.api.nvim_set_keymap('n', '<leader>sc', [[<cmd>lua my_config_files()<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>lr', [[<cmd>lua my_lsp_references()<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>ls', [[<cmd>lua my_lsp_workspace_symbols()<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>sb', [[<cmd>lua my_current_buffer_fuzzy_find()<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>ss', [[<cmd>lua my_resume()<cr>]], opts)
