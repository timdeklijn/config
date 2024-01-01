vim.cmd[[
  au BufNewFile,BufFilePre,BufRead *.html.tera set filetype=html

  setlocal tabstop=2
  setlocal shiftwidth=2
  setlocal expandtab
  setlocal autoindent

  setlocal textwidth=100
]]
