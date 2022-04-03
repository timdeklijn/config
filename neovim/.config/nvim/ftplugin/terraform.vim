setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

" auto format text on save using the LSP
autocmd BufWritePre * :lua vim.lsp.buf.formatting()
