vim.cmd [[autocmd BufWritePre *.rs lua vim.lsp.buf.format()]]
