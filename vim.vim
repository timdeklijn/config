" =============================================================================
" Plugins
" =============================================================================

call plug#begin('~/.vim/plugged')
" Looks
Plug 'sheerun/vim-polyglot'
" Plug 'file://'.expand('~/projects/tim-color')
Plug 'patstockwell/vim-monokai-tasty'
Plug 'itchyny/lightline.vim'

" Handy
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'machakann/vim-highlightedyank'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-markdown'
Plug 'itchyny/vim-gitbranch' " For lightline
Plug 'jmcantrell/vim-virtualenv' " For Lightline

" LSP
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" FZF
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Language specific plugins
Plug 'vim-python/python-syntax'

call plug#end()

" =============================================================================
" Basics
" =============================================================================

" Map leader key
nnoremap <SPACE> <Nop>
let mapleader=" "
" Scrolling is enables
set mouse=a

set number
set laststatus=2
set hidden
set nobackup
set nowritebackup

" Faster timings
set timeout
set ttimeout
set timeoutlen=3000
set ttimeoutlen=50

" Some handy keybindings
nnoremap <silent> <leader><space> :b#<CR>
map <C-n> :NERDTreeToggle<CR>

" =============================================================================
" Set Looks
" =============================================================================

" Show syntax group under cursor
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Needed for colorscheme
if (has("termguicolors"))
  set termguicolors
endif

set t_Co=256
set cursorline
set background=dark
" colorscheme timcolor
colorscheme vim-monokai-tasty

" =============================================================================
" Lightline
" =============================================================================

let g:lightline = {
  \ 'colorscheme' : 'wombat',
  \ 'active' : {
  \   'left': [ [ 'mode', 'paste'],
  \             [ 'gitbranch', 'readonly', 'filename', 'modified', ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'gitbranch#name'
  \ },
  \ }

" =============================================================================
" FZF
" =============================================================================

nnoremap <silent> <leader>ff :GFiles<CR>
nnoremap <silent> <leader>fb :Buffers<CR>
nnoremap <silent> <leader>fw :Windows<CR>
nnoremap <silent> <leader>fl :BLines<CR>
nnoremap <silent> <leader>fg :Ag<CR>

let g:fzf_preview_window = 'right:60%'

" =============================================================================
" File specific settings
" =============================================================================

" markdown --------------------------------------------------------------------
let g:markdown_folding=1 " switch on markdown folding
" Other markdown settings
autocmd FileType markdown
  \ setlocal tabstop=2
    \ shiftwidth=2
    \ softtabstop=2
    \ expandtab
    " set foldlevel in markdown files
    \ foldlevel=1

" python ----------------------------------------------------------------------
autocmd FileType python setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
" vim -------------------------------------------------------------------------
autocmd FileType vim setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
" html ------------------------------------------------------------------------
autocmd FileType html setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType javasript setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType css setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

" =============================================================================
" Language server settings
" =============================================================================

" Copied all settings from the COC git example settings file. Will refine
" when needed

"set cmdheight=2
set updatetime=30
set shortmess+=c

if has("patch-8.1.1563")
  set signcolumn=number
else
  set signcolumn=yes
endif

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-@> coc#refresh()

if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location
" list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>F  <Plug>(coc-format-selected)
nmap <leader>F  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json set formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
