
require'fzf-lua'.setup{
  files = {
    previewer = false
  }
}

local opts = { noremap=true, silent=true }

-- files
vim.api.nvim_set_keymap('n', '<leader>f', "<cmd>lua require('fzf-lua').files()<CR>", opts)

-- config files
vim.api.nvim_set_keymap('n', '<leader>sc', "<cmd>lua require('fzf-lua').files({ cmd = [[rg --color=never --files --hidden --follow -g '!.git']] })<CR>", opts)

-- buffers
vim.api.nvim_set_keymap('n', '<leader><space>', "<cmd>lua require('fzf-lua').buffers()<CR>", opts)

-- :Rg
vim.api.nvim_set_keymap('n', '<leader>sg', "<cmd>lua require('fzf-lua').grep_project()<CR>", opts)

-- References
vim.api.nvim_set_keymap('n', '<leader>sr', "<cmd>lua require('fzf-lua').lsp_references()<CR>", opts)
