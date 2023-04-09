vim.cmd [[autocmd BufWritePre *.go lua vim.lsp.buf.format()]]
vim.cmd [[autocmd BufWritePre *.go lua OrganizeImports(1000)]]

-- Fix golang ugly 8 space tab:
vim.cmd [[
  set autoindent
  set expandtab
  set shiftwidth=4
  set smartindent
  set softtabstop=4
  set tabstop=4
]]

