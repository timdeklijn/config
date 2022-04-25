local opts = { noremap=true, silent=true }

-- open tests in floaterm
vim.cmd[[ let test#strategy = "floaterm" ]]

-- set testrunners per language
vim.cmd[[ let test#python#runner = 'pytest' ]]

-- files
vim.api.nvim_set_keymap('n', '<leader>tn', "<cmd>:TestNearest<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>tt', "<cmd>:TestLast<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>tT', "<cmd>:TestSuite<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>tf', "<cmd>:TestFile<CR>", opts)
vim.api.nvim_set_keymap('n', '<leader>tg', "<cmd>:TestVisit<CR>", opts)
