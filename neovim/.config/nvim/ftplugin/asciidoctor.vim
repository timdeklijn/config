setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

" Conceal on to hide some boilerplate
let g:asciidoctor_syntax_conceal = 1
setlocal conceallevel=3

" show syntax highlighting
let g:asciidoctor_fenced_languages = ['python', 'c', 'javascript', 'json']

" No line numbers
setlocal nonumber
" Turn on spell check
setlocal spell
highlight SpellBad gui=underline,bold guifg=NONE guibg=NONE
