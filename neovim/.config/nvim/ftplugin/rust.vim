" autocmd BufWritePre * :Format
" autocmd BufWritePre * :lua vim.lsp.buf.formatting()<CR>
" autocmd BufWritePre * :lua vim.lsp.buf.formatting()
" autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()


setlocal expandtab
setlocal autoindent

setlocal textwidth=80
