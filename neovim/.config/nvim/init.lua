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
  -- REPL development
  use 'jpalardy/vim-slime'
  -- Go:
  use 'fatih/vim-go'
  use 'buoto/gotests-vim'
  -- Markdown:
  use 'plasticboy/vim-markdown'
  use 'vim-pandoc/vim-pandoc-syntax'
  -- Colors
  use 'Mofiqul/dracula.nvim'
  use 'shaunsingh/nord.nvim'
  use({ "catppuccin/nvim", as = "catppuccin" })
  -- LuaLine
  use 'hoob3rt/lualine.nvim'
  -- File Tree
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  -- Task runner
  use 'skywind3000/asynctasks.vim'
  use 'skywind3000/asyncrun.vim'
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

local color='catppuccin'

if color == 'dracula' then
  vim.cmd[[colorscheme dracula]]
  vim.g.dracula_show_end_of_buffer = false
  vim.g.dracula_transparent_bg = false
  -- set color for: TODO: and NOTE:
  vim.cmd[[ autocmd ColorScheme * highlight TSWarning ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#e68183 gui=bold ]]
  vim.cmd[[ autocmd ColorScheme * highlight TSNote ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#d9bb80 gui=bold ]]
elseif color == 'nord' then
  vim.g.nord_italic = false
  require('nord').set()
elseif color == 'catppuccin' then
  local catppuccin = require('catppuccin')
  catppuccin.setup({
    styles = {
      comments = "NONE",
      functions = "bold",
      keywords = "NONE",
      variables = "NONE",
    }})

  vim.cmd[[ autocmd ColorScheme * highlight commentTSWarning ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#e68183 gui=bold ]]
  vim.cmd[[ autocmd ColorScheme * highlight commentTSNote ctermfg=NONE ctermbg=NONE guibg=NONE guifg=#d9bb80 gui=bold ]]

  vim.cmd[[ colorscheme catppuccin ]]
end


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

-- =============================================================================
-- NVIM TREE
-- =============================================================================

require('nvim-tree').setup({
  nvim_tree_gitignore = true,
  nvim_tree_ignore = { '.git', 'node_modules', '.cache', '__pycache__/'}
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
-- KEYMAPS
-- =============================================================================

-- useful quickfix keybinds
vim.api.nvim_set_keymap('n', '[q', [[:cp<cr>]], opts)
vim.api.nvim_set_keymap('n', ']q', [[:cn<cr>]], opts)
vim.api.nvim_set_keymap('n', '[-', [[:cclose<cr>]], opts)
vim.api.nvim_set_keymap('n', '[+', [[:copen<cr>]], opts)

-- =============================================================================
-- SLIME
-- =============================================================================

-- slime, set the target to tmux to use tmux splits to send code to.
vim.g.slime_target = "tmux"

-- =============================================================================
-- ASYNCTASKS
-- =============================================================================

-- Use asynctasks to run tasks defined in a `.tasks` file. Depending on how the
-- tasks are defined the output is in a quickfix window or in a terminal. The
-- keybinds are all behind `<leader>R`.
vim.cmd[[ let g:asyncrun_open = 6 ]]
vim.cmd[[ let g:asynctasks_term_pos = 'bottom' ]]

vim.api.nvim_set_keymap('n', '<leader>RR', [[<cmd>AsyncTask run<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>Rb', [[<cmd>AsyncTask build<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>Rt', [[<cmd>AsyncTask test<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>Rc', [[<cmd>AsyncTask check<cr>]], opts)
vim.api.nvim_set_keymap('n', '<leader>Rl', [[<cmd>AsyncTaskList<cr>]], opts)

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
require("tim.telescope")
require("tim.lsp_config")
require("tim.cmp")
require("tim.vimwiki")
