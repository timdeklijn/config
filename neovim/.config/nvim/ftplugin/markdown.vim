au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

setlocal textwidth=88

" No line numbers
setlocal nonumber
" Turn on spell check
setlocal spell

let g:markdown_fenced_languages = ['html', 'go', 'python', 'json', 'yaml', "sh"]

" Properly format a list on gq
setlocal formatlistpat=^\\s*\\d\\+[.\)]\\s\\+\\\|^\\s*[*+~-]\\s\\+\\\|^\\(\\\|[*#]\\)\\[^[^\\]]\\+\\]:\\s
setlocal comments=fb:>,fb:*,fb:+,fb:-
setlocal formatoptions+=cn

