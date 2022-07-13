-- DAP settings

local opts = { noremap=true, silent=true }

vim.api.nvim_set_keymap('n', '<leader>dbp', [[<cmd>lua require('dap').toggle_breakpoint()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dc', [[<cmd>lua require('dap').continue()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dsi', [[<cmd>lua require('dap').step_into()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dso', [[<cmd>lua require('dap').step_over()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dr', [[<cmd>lua require('dap').run_last()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dq', [[<cmd>lua require('dap').terminate()<CR>]], opts)

-- PYTHON
-- TODO: move this into python ft
require('dap-python').setup('/Users/timdeklijn/.venvs/debugpy/bin/python')
require('dap-python').test_runner = 'pytest'

-- TODO: figure out if this can be added to the with-args default runner.
table.insert(require("dap").configurations.python, {
  type = "python",
  request = "launch",
  name = "python-from-root",
  program = "${file}",
  args = {"train"},
  cwd = '/Users/timdeklijn/projects/jax',
})

vim.api.nvim_set_keymap('n', '<leader>dn', [[<cmd>lua require('dap-python').test_method()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>df', [[<cmd>lua require('dap-python').test_class()<CR>]], opts)


-- UI
require("dapui").setup()
vim.api.nvim_set_keymap('n', '<leader>duo', [[<cmd>lua require("dapui").open()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>duc', [[<cmd>lua require("dapui").close()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>dut', [[<cmd>lua require("dapui").toggle()<CR>]], opts)
