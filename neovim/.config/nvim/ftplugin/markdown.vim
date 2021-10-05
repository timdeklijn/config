" Open file like a pandoc markdown
autocmd BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
      \ if &ft =~# '^\%(conf\|modula2\)$' |
      \   set ft=markdown.pandoc |
      \ else |
      \   setf markdown |
      \ endif

setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

setlocal textwidth=88
setlocal colorcolumn=88

" Conceal on to hide some boilerplate
setlocal conceallevel=3

" No line numbers
setlocal nonumber
" Turn on spell check
setlocal spell

" Markdown plugin settings:
" No folding
let g:vim_markdown_folding_disabled = 1
" Use conceal
let g:vim_markdown_conceal = 3
" List stuff: Unset options from the markdown plugin
let g:vim_markdown_auto_insert_bullets=0
let g:vim_markdown_new_list_item_indent=0
let g:vim_markdown_conceal_code_blocks=1

let g:markdown_fenced_languages = ['html', 'go', 'python', 'json', 'yaml']

" Properly format a list on gq
setlocal formatlistpat=^\\s*\\d\\+[.\)]\\s\\+\\\|^\\s*[*+~-]\\s\\+\\\|^\\(\\\|[*#]\\)\\[^[^\\]]\\+\\]:\\s
setlocal comments=fb:>,fb:*,fb:+,fb:-
setlocal formatoptions+=cn

nnoremap <silent> <leader>m :Format<CR>
