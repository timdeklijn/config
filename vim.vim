" =============================================================================
" Plugins
" =============================================================================

call plug#begin('~/.vim/plugged')
" Looks
Plug 'sheerun/vim-polyglot'                         " better syntax highlighting
Plug 'itchyny/lightline.vim'                        " status bar
Plug 'lifepillar/vim-solarized8'                    " color theme
Plug 'NLKNguyen/papercolor-theme'

" Handy
Plug 'preservim/nerdcommenter'                      " comment using <leader> cc
Plug 'machakann/vim-highlightedyank'                " show what is yanked
Plug 'christoomey/vim-tmux-navigator'               " use <ctrl> + hjkl to move around all splits
Plug 'tpope/vim-surround'                           " surround with quotes/brackets everything
Plug 'plasticboy/vim-markdown'                      " markdown plugin
Plug 'itchyny/vim-gitbranch'                        " For lightline git status
Plug 'direnv/direnv.vim'                            " use 'local' conf
Plug 'osyo-manga/vim-over'                          " highlight while searching

" LSP
Plug 'neoclide/coc.nvim', {'branch': 'release'}     " language server client

" FZF
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } } " install fzf
Plug 'junegunn/fzf.vim'                             " vim bindings to fzf

call plug#end()

" =============================================================================
" Basics
" =============================================================================

" Map leader key
nnoremap <SPACE> <Nop>
let mapleader=" "
" Scrolling is enabled
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
set updatetime=300
set shortmess+=c

" Sane search/replace
set hlsearch
set incsearch

if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Some handy keybindings
nnoremap <silent> <leader><space> :b#<CR>

" ':W' works as ':w'
command W w

" Set cursor
let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"

" =============================================================================
" Set Looks
" =============================================================================

set cursorline
set termguicolors
colorscheme PaperColor
highlight Normal guibg=NONE ctermbg=NONE
highlight LineNr guibg=NONE ctermbg=NONE
highlight SignColumn guibg=NONE ctermbg=NONE
highlight NonText guibg=None ctermbg=None
 

" =============================================================================
" Lightline
" =============================================================================

let g:lightline = {
  \ 'colorscheme': 'PaperColor',
  \ 'active' : {
  \   'left': [ [ 'mode', 'paste'],
  \             [ 'filename', 'modified', 'gitbranch', 'readonly' ] ]
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

" Other tabbing
autocmd FileType markdown
  \ setlocal tabstop=2
    \ shiftwidth=2
    \ softtabstop=2
    \ expandtab
    
" This conceals links in markdown text.
let g:vim_markdown_conceal=1
let g:vim_markdown_folding_disabled=1
set conceallevel=2
let g:vim_markdown_conceal_code_blocks=0

let g:vim_markdown_auto_insert_bullets=0
let g:vim_markdown_new_list_item_indent=0

" Let lists be properly formatted
au FileType markdown 
  \ setlocal formatlistpat=^\\s*\\d\\+[.\)]\\s\\+\\\|^\\s*[*+~-]\\s\\+\\\|^\\(\\\|[*#]\\)\\[^[^\\]]\\+\\]:\\s |
  \ setlocal comments=n:> |
  \ setlocal formatoptions+=cn

" Go -------------------------------------------------------------------------
autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
" python ----------------------------------------------------------------------
autocmd FileType python setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
" vim -------------------------------------------------------------------------
autocmd FileType vim setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
" html ------------------------------------------------------------------------
autocmd FileType html setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType javasript setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType css setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

" LSP ------------------------------------------------------------------------
"
" Configs are copied from coc-vim repository
"
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <c-@> coc#refresh()

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
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
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
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

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
