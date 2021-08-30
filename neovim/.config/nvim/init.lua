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
  use 'tpope/vim-fugitive'

  -- UI to select things (files, grep results, open buffers...)
  use {'nvim-telescope/telescope.nvim', 
    requires = {
      {'nvim-lua/popup.nvim'},
      {'nvim-lua/plenary.nvim'}
    }}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

  -- Add indentation guides even on blank lines
  use { 'lukas-reineke/indent-blankline.nvim' }
  -- Add git related info in the signs columns and popups
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'} }
  use 'neovim/nvim-lspconfig'        -- Collection of configurations for built-in LSP client
  use 'hrsh7th/nvim-compe'           -- Autocompletion plugin
  -- Tree sitter
  use 'nvim-treesitter/nvim-treesitter'

  -- Environment setup
  use 'direnv/direnv.vim' 
  use 'christoomey/vim-tmux-navigator'

  -- Language packages --------------------------------------------------------
  use 'mhartington/formatter.nvim'

  -- Go:
  use 'fatih/vim-go'
  use 'buoto/gotests-vim'

  -- Asciidoctor:
  use 'habamax/vim-asciidoctor'

  -- Markdown:
  use 'plasticboy/vim-markdown'
  use 'vim-pandoc/vim-pandoc-syntax'

  use 'vimwiki/vimwiki'

  -- Colors -------------------------------------------------------------------
  use 'EdenEast/nightfox.nvim'

  -- LuaLine ------------------------------------------------------------------
  use 'hoob3rt/lualine.nvim'

  -- Editing ------------------------------------------------------------------
  use 'godlygeek/tabular'

  -- File Tree ----------------------------------------------------------------
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'

  -- TODOS --------------------------------------------------------------------
  use {
  "folke/todo-comments.nvim",
  requires = "nvim-lua/plenary.nvim",
  config = function()
    require("todo-comments").setup { }
  end
}
end)

--Incremental live completion
vim.o.inccommand = "nosplit"

--Set highlight on search
vim.o.hlsearch = false
vim.o.incsearch = true

--Make line numbers default
vim.wo.number = true

--Do not save when switching buffers
vim.o.hidden = true

--Enable mouse mode
vim.o.mouse = "a"

--Enable break indent
vim.o.breakindent = true

--Save undo history
vim.cmd[[set undofile]]

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = "yes"

-- Copy paste from system clipboard
vim.cmd[[ set clipboard=unnamedplus ]]

-- highlight current line
vim.cmd[[set cursorline]]

-- Treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  highlight = {
    enable = true
  }
}

--Set colorscheme
vim.o.termguicolors = true

local nightfox = require('nightfox')
nightfox.setup({
  fox = "nightfox", -- change the colorscheme to use nordfox
  transparent = true,
  styles = {
    comments = "NONE", -- change style of comments to be italic
    keywords = "bold", -- change style of keywords to be bold
    functions = "bold" -- styles can be a comma separated list
  },
})

-- Load the configuration set above and apply the colorscheme
nightfox.load()

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

-- Toggle to disable mouse mode and indentlines for easier paste
ToggleMouse = function()
  if vim.o.mouse == 'a' then
    vim.cmd[[IndentBlanklineDisable]]
    vim.wo.signcolumn='no'
    vim.o.mouse = 'v'
    vim.wo.number = false
    print("Mouse disabled")
  else
    vim.cmd[[IndentBlanklineEnable]]
    vim.wo.signcolumn='yes'
    vim.o.mouse = 'a'
    vim.wo.number = true
    print("Mouse enabled")
  end
end

vim.api.nvim_set_keymap('n', '<F10>', '<cmd>lua ToggleMouse()<cr>', { noremap = true })

-- Change preview window location
vim.g.splitbelow = true

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
vim.cmd[[ nnoremap <C-n> :NvimTreeToggle<CR> ]]
vim.cmd[[ nnoremap <leader>r :NvimTreeRefresh<CR> ]]
vim.cmd[[ nnoremap <leader>n :NvimTreeFindFile<CR> ]]
vim.cmd[[ let g:nvim_tree_ignore = [ '.git', 'node_modules', '.cache', '__pycache__/' ] ]]
vim.cmd[[ let g:nvim_tree_gitignore=1 ]]
vim.cmd[[ let g:nvim_tree_lsp_diagnostics = 1 ]]

-- fugitive shortcuts
vim.api.nvim_set_keymap('n', '<leader>gg', [[:Git <CR>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gp', [[:Git push<CR>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gP', [[:Git pull<CR>]], { noremap = true, silent = true})
-- Telescope git commands:
-- <leader>gb branches
-- <leader>ga all commits
-- <leader>gc commits for file

-- Load external configs
require("tim.lualine")
require("tim.telescope")
require("tim.lsp_config")
require("tim.formatter")
require("tim.compe")
require("tim.vimwiki")
