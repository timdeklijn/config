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
  use 'tpope/vim-fugitive'           -- git
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim'}}}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
  use 'ibhagwan/fzf-lua'             -- search all the things
  -- Add indentation guides even on blank lines
  use { 'lukas-reineke/indent-blankline.nvim' }
  -- Add git related info in the signs columns and popups
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'} }
 -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'        
  use 'jose-elias-alvarez/null-ls.nvim'
  use "ray-x/lsp_signature.nvim"
  -- Autocomplete using cmp
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'
  -- Tree sitter
  use 'nvim-treesitter/nvim-treesitter'
  -- Environment setup
  use 'direnv/direnv.vim' 
  use 'christoomey/vim-tmux-navigator'
  -- Debugging
  use 'mfussenegger/nvim-dap'
  use 'mfussenegger/nvim-dap-python'
  use 'rcarriga/nvim-dap-ui'
  -- Go:
  use 'fatih/vim-go'
  use 'buoto/gotests-vim'
  -- Rust
  use 'simrat39/rust-tools.nvim'
  -- Scala
  use({'scalameta/nvim-metals', requires = { "nvim-lua/plenary.nvim" }})
  -- Tests
  use 'vim-test/vim-test'
  -- Terminal
  use 'voldikss/vim-floaterm'
  -- Colors
  use 'sainnhe/everforest'
  use 'projekt0n/github-nvim-theme'
  -- LuaLine
  use 'hoob3rt/lualine.nvim'
  -- File Tree
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  -- VimWiki
  use 'vimwiki/vimwiki'
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
  ensure_installed = "all",
  highlight = {
    enable = true,
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

require('github-theme').setup({
  theme_style = "dark_default",
  function_style = "bold",
  comment_style = "NONE",
  keyword_style = "NONE",
})

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

require("indent_blankline").setup {
    show_current_context = true,
}

-- Highlight on yank
vim.api.nvim_exec([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)

-- =============================================================================
-- GIT SIGNS
-- =============================================================================

require('gitsigns').setup()

-- =============================================================================
-- NVIM TREE
-- =============================================================================

require('nvim-tree').setup({
  git = {
    enable = true,
    ignore = true,
    timeout = 400,
  },
  view = {
    side = 'right'
  }
})
vim.cmd[[ nnoremap <C-n> :NvimTreeToggle<CR> ]]
vim.cmd[[ nnoremap <leader>r :NvimTreeRefresh<CR> ]]
vim.cmd[[ nnoremap <leader>n :NvimTreeFindFile<CR> ]]

-- =============================================================================
-- FUGITIVE
-- =============================================================================

local opts = { noremap = true, silent = true}
vim.api.nvim_set_keymap('n', '<leader>gg', [[:Git <CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>gp', [[:Git push<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>gP', [[:Git pull<CR>]], opts)

-- =============================================================================
-- QUICKFIX KEYMAPS
-- =============================================================================

vim.api.nvim_set_keymap('n', '[q', [[:cp<cr>]], opts)
vim.api.nvim_set_keymap('n', ']q', [[:cn<cr>]], opts)
vim.api.nvim_set_keymap('n', '[-', [[:cclose<cr>]], opts)
vim.api.nvim_set_keymap('n', '[+', [[:copen<cr>]], opts)

-- =============================================================================
-- LSP SIGNATURE
-- =============================================================================

require "lsp_signature".setup({
  floating_window = true,
})

-- =============================================================================
-- METALS
-- =============================================================================

local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "scala", "sbt", "java" },
  callback = function()
    require("metals").initialize_or_attach({})
  end,
  group = nvim_metals_group,
})

metals_config = require("metals").bare_config()
metals_config.init_options.statusBarProvider = "on"

-- =============================================================================
-- Rust
-- =============================================================================

local opts = {
    dap = {
      adapter = {
        type = "executable",
        command = "lldb-vscode",
        name = "rt_lldb",
    },
  },
}
require('rust-tools').setup(opts)

-- =============================================================================
-- IMPORT CONFIGS
-- =============================================================================

-- Load external configs
require("tim.lualine")
require("tim.vim_test")
require("tim.lsp_config")
require("tim.cmp")
require("tim.telescope")
require("tim.dap")
