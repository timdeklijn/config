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
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

  -- Add indentation guides even on blank lines
  use { 'lukas-reineke/indent-blankline.nvim' }
  -- Add git related info in the signs columns and popups
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'} }
  use 'neovim/nvim-lspconfig'        -- Collection of configurations for built-in LSP client

  -- completion
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'

  -- LuaSnip
  use 'L3MON4D3/LuaSnip'
  use "rafamadriz/friendly-snippets"
  use 'saadparwaiz1/cmp_luasnip'

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
  use 'shaunsingh/nord.nvim' 
  use 'lifepillar/vim-gruvbox8'
  use 'Shatur/neovim-ayu'
  use 'Mofiqul/dracula.nvim'
  use 'RRethy/nvim-base16'

  -- LuaLine ------------------------------------------------------------------
  use 'hoob3rt/lualine.nvim'

  -- Editing ------------------------------------------------------------------
  use 'godlygeek/tabular'

  -- File Tree ----------------------------------------------------------------
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'

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

-- vim.cmd[[ let g:gruvbox_bold = 1 ]]
-- vim.cmd[[ let g:gruvbox_filetype_hi_groups = 1 ]]
-- vim.cmd[[ let g:gruvbox_italics = 0 ]]
-- vim.cmd[[ let g:gruvbox_plugin_hi_groups = 1 ]]
-- vim.cmd[[ let g:gruvbox_transp_bg = 1 ]] 

-- vim.cmd[[ colorscheme nord ]]
-- vim.cmd[[ colorscheme jellybeans-nvim ]]
-- vim.g.nord_contrast = false
-- vim.g.nord_borders = true
-- vim.g.nord_disable_background = false
-- vim.g.nord_italic = false

-- I really like Treesitter, and I can use the highlight groups to make
-- specific groups dispolay as bold.

-- =============================================================================
-- COLORS
-- =============================================================================

vim.cmd[[ colorscheme base16-tomorrow-night ]]
vim.cmd[[ autocmd ColorScheme * highlight TSFunction gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSConstant gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSType gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TODO gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSConstant gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSOperator gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSKeyword gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSKeywordOperator gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSKeywordFunction gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSKeywordReturn gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSRepeat gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSMethod gui=bold ]]
vim.cmd[[ autocmd ColorScheme * highlight TSBoolean gui=bold ]]

-- require('colorbuddy').colorscheme('timbeans')
--vim.cmd[[colorscheme dracula]]

------ Fixup color scheme. To get bold highlights the colorscheme should be 
------ overwritten. To prevent overwriting the colors, we need to save them
------ and reapply them when making the highlights bold.
------
------ get the guifg by checking the output of the `:hi` command
--local c1 = '#F1FA8C' -- TSFunction
--local c2 = '#F1FA8C' -- TSConstant
--local c3 = '#FF79C6' -- TSType
--local c4 = '#BD93F9' -- TODO
--local c5 = '#FF79C6' -- TSConditional
--local c6 = '#BD93F9' -- All keywords
--local c7 = '#50fa7b'
---- List of color 'corrections'
---- TODO: this can be made into a function

--local recolors = {
--  {"TSFunction", c1},
--  {"TSConstant", c2},
--  {"TSType", c3},
--  {"TODO", c4},
--  {"TSConditional", c5},
--  {"TSOperator", c6}, 
--  {"TSKeyword", c6},
--  {"TSKeywordOperator", c6},
--  {"TSKeywordFunction", c6},
--  {"TSKeywordReturn", c6},
--  {"TSRepeat", c6},
--  {"TSMethod", c7},
--  {"TSBoolean", c4},
--}

--local color_string = function(group, color)
--  return 'autocmd ColorScheme * highlight ' .. group .. ' gui=bold, guifg=' .. color
--end

---- Recolor
--for i, v in ipairs(recolors) do
--  vim.cmd(color_string(v[1], v[2]))
--end

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
require('nvim-tree').setup()
vim.cmd[[ nnoremap <C-n> :NvimTreeToggle<CR> ]]
vim.cmd[[ nnoremap <leader>r :NvimTreeRefresh<CR> ]]
vim.cmd[[ nnoremap <leader>n :NvimTreeFindFile<CR> ]]
vim.cmd[[ let g:nvim_tree_ignore = [ '.git', 'node_modules', '.cache', '__pycache__/' ] ]]
vim.cmd[[ let g:nvim_tree_gitignore=1 ]]
-- vim.cmd[[ let g:nvim_tree_lsp_diagnostics = 1 ]]

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

-- Load external configs
require("tim.lualine")
require("tim.telescope")
require("tim.lsp_config")
require("tim.formatter")
require("tim.cmp")
require("tim.vimwiki")
