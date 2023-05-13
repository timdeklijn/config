vim.cmd [[
setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent
]]

vim.cmd [[autocmd BufWritePre *.tf lua vim.lsp.buf.format() ]]
