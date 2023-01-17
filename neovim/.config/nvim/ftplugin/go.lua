vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
vim.cmd [[autocmd BufWritePre * lua OrganizeImports(1000)]]

-- Fix golang ugly 8 space tab:
vim.cmd [[
  set autoindent
  set expandtab
  set shiftwidth=4
  set smartindent
  set softtabstop=4
  set tabstop=4
]]

