-- TELESCOPE --

require('telescope').setup{
  extensions = {
    fzf = {
      fzf = true,
      override_generic_sorter = true,
      override_file_sorder = true,
      case_mode = "smart_case",
    }
  },
  pickers = {
    buffers = {
      ignore_current_buffer = true,
      sort_lastused = true,
    },
  }
}

require('telescope').load_extension('fzf')

local opts = { noremap=true, silent=true }

vim.api.nvim_set_keymap('n', 
  '<leader>f', "<cmd>lua require'telescope.builtin'.find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>",
  opts)
vim.api.nvim_set_keymap('n', '<leader><space>', "<cmd>lua require('telescope.builtin').buffers()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>sg', "<cmd>lua require('telescope.builtin').live_grep()<CR>", opts)
