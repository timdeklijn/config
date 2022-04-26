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
  use 'ibhagwan/fzf-lua'             -- search all the things
  -- Add indentation guides even on blank lines
  use { 'lukas-reineke/indent-blankline.nvim' }
  -- Add git related info in the signs columns and popups
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'} }
 -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'        
  use 'jose-elias-alvarez/null-ls.nvim'
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
  -- Go:
  -- TODO: at some point this should all simply be handled by LSP
  use 'fatih/vim-go'
  use 'buoto/gotests-vim'
  -- Tests
  use 'vim-test/vim-test'
  -- Terminal
  use 'voldikss/vim-floaterm'
  -- Colors
  use({
    "catppuccin/nvim",
    as = "catppuccin"
  })
  -- LuaLine
  use 'hoob3rt/lualine.nvim'
  -- File Tree
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  use {"nvim-neorg/neorg", requires = "nvim-lua/plenary.nvim"}
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
-- vim.cmd[[ set background=light ]]
vim.cmd[[ set background=dark ]]
local catppuccin = require("catppuccin")

-- configure it
catppuccin.setup({
  styles = {
    comments = "NONE",
    functions = "bold",
    keywords = "bold",
    strings = "NONE",
    variables = "NONE",
  }
})
vim.cmd[[colorscheme catppuccin]]

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
  nvim_tree_gitignore = true,
  nvim_tree_ignore = { '.git', 'node_modules', '.cache', '__pycache__/'},
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

-- useful quickfix keybinds
vim.api.nvim_set_keymap('n', '[q', [[:cp<cr>]], opts)
vim.api.nvim_set_keymap('n', ']q', [[:cn<cr>]], opts)
vim.api.nvim_set_keymap('n', '[-', [[:cclose<cr>]], opts)
vim.api.nvim_set_keymap('n', '[+', [[:copen<cr>]], opts)

-- =============================================================================
-- NEORG
-- =============================================================================

require('neorg').setup {
    load = {
        ["core.defaults"] = {},
        ["core.norg.concealer"] = {}
    }
}

-- =============================================================================
-- CUSTOM FUNCTIONS
-- =============================================================================

T = {}

-- new_note creates a not in my notes folder with an id based on datetime and a
-- title prompted from the user. The id and title are used in the filename as
-- well as the header written to the file.
T.new_note = function()
  -- where to save the note
  local notes_dir = "/Users/timdeklijn/wiki/"

  -- create ID for current datetime: YYYYMMDDHHMM
  local c = os.date("*t")
  local id = (tostring(c.year) .. tostring(c.month) .. 
              tostring(c.day) .. tostring(c.hour) .. 
              tostring(c.min))

  -- get title from user prompt
  local raw_title = vim.fn.input("Title: ")

  -- filename from id and title
  file_title = string.lower(string.gsub(raw_title, " ", "_"))
  local filename = notes_dir .. id .. "_" .. file_title .. ".md"
  
  -- file title from id and title
  local header_title = "# " .. raw_title

  -- create an empty buffer, and focus the current window to it. Write the
  -- title to it and save the file to the notes folder
  local buf = vim.api.nvim_create_buf(false, false)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {header_title})
  vim.api.nvim_command("write" .. " " .. filename)
end

-- Keybind to create a new note
vim.api.nvim_set_keymap('n', '<leader>NN', [[<cmd>lua T.new_note()<cr>]], opts)
vim.api.nvim_set_keymap(
  'n', 
  '<leader>NT', 
  [[<cmd>lua vim.api.nvim_command("edit ~/wiki/todo.md")<cr>]], 
  opts)

-- =============================================================================
-- IMPORT CONFIGS
-- =============================================================================

-- Load external configs
require("tim.lualine")
require("tim.vim_test")
require("tim.lsp_config")
require("tim.cmp")
require("tim.fzf")
