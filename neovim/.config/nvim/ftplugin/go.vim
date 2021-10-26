setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab
setlocal autoindent

" Use this to autowrap text
setlocal textwidth=80


" Run test under cursor:
nnoremap <leader>tf :GoTestFunc<CR>
" Run all tests in file:
nnoremap <leader>ta :GoTest<CR>
" Create test for function under cursor:
nnoremap <leader>tm :GoTests<CR>
