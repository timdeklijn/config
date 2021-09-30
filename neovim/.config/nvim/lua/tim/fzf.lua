-- My config for telescope
--
-- Tim de Klijn, 2021

local opts = { noremap=true, silent=true }

-- files
vim.api.nvim_set_keymap('n', '<leader>f', "<cmd>lua require('fzf-lua').files()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader><space>', "<cmd>lua require('fzf-lua').buffers()<CR>", opts)

-- git
vim.api.nvim_set_keymap('n', '<leader>ga', "<cmd>lua require('fzf-lua').git_commits()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>gc', "<cmd>lua require('fzf-lua').git_bcommits()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>gb', "<cmd>lua require('fzf-lua').git_branches()<CR>", opts)

-- greps
vim.api.nvim_set_keymap('n', '<leader>ssw', "<cmd>lua require('fzf-lua').grep_cword()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>ssf', "<cmd>lua require('fzf-lua').blines()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>ssg', "<cmd>lua require('fzf-lua').live_grep()<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>ssr', "<cmd>lua require('fzf-lua').live_grep_resume()<CR>", opts)
