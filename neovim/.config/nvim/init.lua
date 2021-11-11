-- Install packer
local execute = vim.api.nvim_command

local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '.. install_path)
end

vim.api.nvim_exec([[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]], false)

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim'       -- Package manager
  use 'tpope/vim-commentary'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-fugitive'           --git
  -- Telescope to navigate and do some other cool search things
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim'}}}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
  -- Add indentation guides even on blank lines
  use { 'lukas-reineke/indent-blankline.nvim' }
  -- Add git related info in the signs columns and popups
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'} }
  -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'        
  use 'jose-elias-alvarez/null-ls.nvim'
  -- Autocomplete
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  -- Tree sitter
  use 'nvim-treesitter/nvim-treesitter'
  -- Environment setup
  use 'direnv/direnv.vim' 
  use 'christoomey/vim-tmux-navigator'
  -- REPL development
  use 'jpalardy/vim-slime'
  -- Go:
  use 'fatih/vim-go'
  use 'buoto/gotests-vim'
  -- Asciidoctor:
  use 'habamax/vim-asciidoctor'
  -- Markdown:
  use 'plasticboy/vim-markdown'
  use 'vim-pandoc/vim-pandoc-syntax'
  -- Colors
  use 'sainnhe/everforest'
  use 'sainnhe/gruvbox-material'
  use 'bluz71/vim-moonfly-colors'
  -- LuaLine
  use 'hoob3rt/lualine.nvim'
  -- Editing
  use 'godlygeek/tabular'
  -- File Tree
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
end)

-- =============================================================================
-- General Settings
-- =============================================================================

vim.o.inccommand = "nosplit"
vim.o.hlsearch = false
vim.o.incsearch = true
vim.wo.number = true
vim.o.hidden = true
vim.o.mouse = "a"
vim.o.breakindent = true
vim.cmd[[set undofile]]
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.updatetime = 250
vim.wo.signcolumn = "yes"
vim.cmd[[ set clipboard=unnamedplus ]]
vim.cmd[[set cursorline]]
vim.g.splitbelow = true

-- =============================================================================
-- TREESITTER
-- =============================================================================

-- Treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  highlight = {
    enable = true
  }
}

-- Treesitter based folder
vim.o.foldmethod='expr'
vim.o.foldenable = false
vim.o.foldexpr='nvim_treesitter#foldexpr()'

-- =============================================================================
-- COLORS
-- =============================================================================

vim.o.termguicolors = true

-- Set colorscheme and prevent italics
vim.cmd[[ let g:gruvbox_material_background = 'hard' ]]
vim.cmd[[ let g:gruvbox_material_enable_italic = 0 ]]
vim.cmd[[ let g:gruvbox_material_disable_italic_comment = 0 ]]
vim.cmd[[ let g:gruvbox_material_show_eob = 0  ]]
vim.cmd[[ let g:gruvbox_material_statusline_style = 'original' ]]
vim.cmd[[ colorscheme gruvbox-material ]]

-- Remove foreground color from TODO: and NOTE:
vim.cmd[[ autocmd ColorScheme * highlight TSWarning ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#e68183 gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSNote ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#d9bb80 gui=bold ]]

-- =============================================================================
-- REMAPS
-- =============================================================================

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent=true})
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- I can't type, map :Q and :W to :q and :w
vim.api.nvim_exec([[
  command W w
  command Q q
]], false)

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

--Remap escape to leave terminal mode
vim.api.nvim_exec([[
  augroup Terminal
    autocmd!
    au TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
    au TermOpen * set nonu
  augroup end
]], false)

--Add map to enter paste mode
vim.o.pastetoggle="<F3>"

--Map blankline
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'

-- Highlight on yank
vim.api.nvim_exec([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)

-- Y yank until the end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true})

-- Show git info in the gutter
require('gitsigns').setup()

-- Nvim tree
require('nvim-tree').setup({
  nvim_tree_gitignore = true,
  nvim_tree_ignore = { '.git', 'node_modules', '.cache', '__pycache__/'}
})
vim.cmd[[ nnoremap <C-n> :NvimTreeToggle<CR> ]]
vim.cmd[[ nnoremap <leader>r :NvimTreeRefresh<CR> ]]
vim.cmd[[ nnoremap <leader>n :NvimTreeFindFile<CR> ]]

-- fugitive shortcuts
local opts = { noremap = true, silent = true}
vim.api.nvim_set_keymap('n', '<leader>gg', [[:Git <CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>gp', [[:Git push<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>gP', [[:Git pull<CR>]], opts)

-- useful quickfix keybinds
vim.api.nvim_set_keymap('n', '[q', [[:cp<cr>]], opts)
vim.api.nvim_set_keymap('n', ']q', [[:cn<cr>]], opts)
vim.api.nvim_set_keymap('n', '[-', [[:cclose<cr>]], opts)
vim.api.nvim_set_keymap('n', '[+', [[:copen<cr>]], opts)

-- slime, set the target to tmux to use tmux splits to send code to.
vim.g.slime_target = "tmux"

-- Load external configs
require("tim.lualine")
require("tim.telescope")
require("tim.lsp_config")
require("tim.cmp")
require("tim.vimwiki")
