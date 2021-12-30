autocmd BufWritePre *.rs :Format
autocmd BufWritePre *.rs :lua vim.lsp.buf.formatting()

setlocal expandtab
setlocal autoindent

setlocal textwidth=80
