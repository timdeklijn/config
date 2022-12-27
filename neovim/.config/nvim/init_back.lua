-- TODO:
--   - [X] function args plugin:   use "ray-x/lsp_signature.nvim"
--   - [X] nvim-tree
--   - [X] use { 'lukas-reineke/indent-blankline.nvim' }
--   - [X] gitsigns
--   - [X] null-ls -> diagnostics
--   - [X] neogit
--   - [ ] dap
--   - [ ] neotest ?
local ensure_packer = function()
  local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
  if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

-- Load all plugins
require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- Base plugins
  use 'tpope/vim-commentary' -- TODO: look into nvim commentary
  use 'kylechui/nvim-surround'
  use 'christoomey/vim-tmux-navigator'
  use 'direnv/direnv.vim'

  -- Telescope
  use { 'nvim-telescope/telescope.nvim',
    requires = { { 'nvim-lua/plenary.nvim' } }
  }
  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

  -- Looks
  use "rebelot/kanagawa.nvim"
  use 'nvim-lualine/lualine.nvim'
  use 'kyazdani42/nvim-web-devicons'
  use { 'lukas-reineke/indent-blankline.nvim' }

  -- Treesitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use 'nvim-treesitter/playground'

  -- LSP
  use 'neovim/nvim-lspconfig'
  use "ray-x/lsp_signature.nvim"
  use "jose-elias-alvarez/null-ls.nvim"

  -- Autocomplete
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'hrsh7th/nvim-cmp'
  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'

  -- File tree
  use 'nvim-tree/nvim-tree.lua'

  -- Git
  use 'TimUntersberger/neogit'
  use 'lewis6991/gitsigns.nvim'

  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- Some sensible setup ---------------------------------------------------------
-- TODO: move this into file

vim.o.inccommand = "nosplit"
vim.o.hlsearch = false
vim.o.incsearch = true
vim.wo.number = true
vim.o.hidden = true
vim.o.mouse = "a"
vim.o.breakindent = true
vim.cmd [[set undofile]]
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.updatetime = 250
vim.wo.signcolumn = "yes"
vim.cmd [[ set clipboard=unnamedplus ]]
vim.cmd [[set cursorline]]
vim.g.splitbelow = true

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- For nvim-tree:
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.opt.termguicolors = true

-- Customize the look of Neovim -----------------------------------------------

-- Kanagawa: Colorscheme
require('kanagawa').setup({
  undercurl = false, -- enable undercurls
  commentStyle = { italic = false },
  functionStyle = { bold = true },
  keywordStyle = { italic = false, bold = true },
  statementStyle = { bold = true },
  typeStyle = { bold = true },
  variablebuiltinStyle = { italic = false },
  specialReturn = true, -- special highlight for the return keyword
  specialException = true, -- special highlight for exception handling keywords
  transparent = false, -- do not set background color
  dimInactive = false, -- dim inactive window `:h hl-NormalNC`
  globalStatus = false, -- adjust window separators highlight for laststatus=3
  terminalColors = true, -- define vim.g.terminal_color_{0,17}
  colors = {},
  overrides = {},
  theme = "default" -- Load "default" theme or the experimental "light" theme
})
vim.cmd [[colorscheme kanagawa]]

-- LuaLine: modeline on the bottom of the screen.
local lualine = require("lualine")
lualine.setup({
  options = {
    theme = "kanagawa",
  }
})

-- TreeSitter: use grammar parser
require('nvim-treesitter.configs').setup({
  ensure_installed = "all",
  highlight = {
    enable = true
  }
})

-- Show indent guides. The current context of the cursor will be highlighted
-- differently.
require("indent_blankline").setup {
  show_current_context = true,
}

-- An autocommand that will hightlight the yanked region when a yank has been
-- performed.
vim.api.nvim_create_autocmd(
  "TextYankPost",
  { command = "lua vim.highlight.on_yank()" }
)

-- Create autocommands to show cursorlines in active windows/buffers, but hide
-- them when leaving the window.
vim.api.nvim_create_autocmd(
  { "VimEnter", "WinEnter", "BufWinEnter" },
  { command = "setlocal cursorline" }
)
vim.api.nvim_create_autocmd(
  "WinLeave",
  { command = "setlocal nocursorline" }
)

-- Base Plugins ----------------------------------------------------------------

-- use motions to modify surrounding characters
require("nvim-surround").setup()

-- GIT -------------------------------------------------------------------------

-- neogit is a magit clone for NeoVim and it looks great.
local neogit = require('neogit')
neogit.setup {}
vim.keymap.set("n", "<leader>gg", neogit.open, {})

-- Gitsigns show lines that have been edited compared to the last commit.
local gitsigns = require("gitsigns")
gitsigns.setup()
-- Toggle line blame will show who made the last edit of a line.
vim.keymap.set("n", "<leader>gb", gitsigns.toggle_current_line_blame, {})

-- NVIM-TREE -------------------------------------------------------------------
-- File broweser. Can be toggled with C-n.

local nvimTree = require("nvim-tree")
nvimTree.setup {
  git = {
    enable = true,
    ignore = false,
    timeout = 400,
  },
  view = {
    side = "right",
  },
}

vim.keymap.set("n", "<C-n>", require("nvim-tree.api").tree.toggle, {})

-- Telescope ------------------------------------------------------------------
-- A finder tool to locate all the things within a project.
--
-- TODO: move to custom setup file/function

local telescope = require("telescope")
local telescopeConfig = require("telescope.config")
local builtin = require('telescope.builtin')
local vimgrep_arguments = { unpack(telescopeConfig.values.vimgrep_arguments) }

-- Modify grep argments
table.insert(vimgrep_arguments, "--hidden")
table.insert(vimgrep_arguments, "--glob")
table.insert(vimgrep_arguments, "!**/.git/*")

telescope.setup({
  -- Overwrite defaults
  defaults = {
    vimgrep_arguments = vimgrep_arguments,
  },

  -- Customize pickers
  pickers = {
    find_files = {
      find_command = { "rg", "--files", "--hidden", "--glob", "!**/.git/*" },
    },
  },

  -- Setup extensions
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    }
  },
})

-- Load extensions
telescope.load_extension('fzf')

-- Keymaps
vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>sg', builtin.live_grep, {})
vim.keymap.set('n', '<leader><leader>', builtin.buffers, {})

-- LSP -------------------------------------------------------------------------
-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '=', function() vim.lsp.buf.format { async = true } end, bufopts)
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local lsp_flags = {
  debounce_text_changes = 150,
}

local null_ls = require("null-ls")
null_ls.setup({
  sources = {
    -- python needs A LOT of diagnostics
    null_ls.builtins.diagnostics.flake8,
    null_ls.builtins.formatting.black,
    null_ls.builtins.diagnostics.mypy
  },
})

-- Show a function signature when in insert mode and the function is called.
require "lsp_signature".setup({
  floating_window = true,
})

-- -----------------------------------------------------------------------------
-- Register language servers
-- -----------------------------------------------------------------------------
-- TODO: terraform, yaml, markdown

require('lspconfig')['pyright'].setup {
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
}

require('lspconfig')['gopls'].setup {
  on_attach = on_attach,
  flags = lsp_flags,
}

-- `brew install lua-language-server`
require('lspconfig')['sumneko_lua'].setup {
  on_attach = on_attach,
  flags = lsp_flags,
}

require('lspconfig')['rust_analyzer'].setup {
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
}

-- CMP -------------------------------------------------------------------------
-- Completion engine. Will show possible completions.

local cmp = require("cmp")

local kind_icons = {
  Text = "",
  Method = "",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "ﴯ",
  Interface = "",
  Module = "",
  Property = "ﰠ",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = ""
}

cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  window = {
    completion = {
      border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
      winhighlight = "Normal:CmpPmenu,FloatBorder:CmpPmenuBorder,CursorLine:PmenuSel,Search:None",
    },
    documentation = {
      border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
      winhighlight = "Normal:CmpPmenu,FloatBorder:CmpPmenuBorder,CursorLine:PmenuSel,Search:None",
    },
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = 'treesitter' },
    { name = "luasnip" },
    { name = 'path', option = { trailing_slash = true } },
    { name = 'nvim_lsp_signature_help' },
  }, {
    { name = "buffer" },
  }),
  formatting = {
    format = function(entry, vim_item)
      vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind)
      vim_item.menu = ({
        buffer = "[Buffer]",
        nvim_lsp = "[LSP]",
        luasnip = "[LuaSnip]",
        nvim_lua = "[Lua]",
        latex_symbols = "[LaTeX]",
      })[entry.source.name]
      return vim_item
    end
  },
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won"t work anymore).
cmp.setup.cmdline({ "/", "?" }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = "buffer" }
  }
})

-- Use cmdline & path source for ":" (if you enabled `native_menu`, this won"t work anymore).
cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" }
  }, {
    { name = "cmdline" }
  })
})
