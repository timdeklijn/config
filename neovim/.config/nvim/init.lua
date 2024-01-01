-- Set <space> as the leader key
-- See `:help mapleader`
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
require('lazy').setup({
  -- Git related plugins
  {
    'tpope/vim-fugitive',
    keys = {
      { "<leader>gg", "<cmd>Git<cr>",      desc = "[G]it" },
      { "<leader>gp", "<cmd>Git push<cr>", desc = "[G]it" },
    },
  },
  'tpope/vim-rhubarb',

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', tag = 'legacy', opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
  },

  {
    'mfussenegger/nvim-lint',
    ft = { 'python' },
    config = function()
      require('lint').linters_by_ft = {
        python = { 'pylint', 'mypy' },
      }
    end
  },

  {
    "stevearc/conform.nvim",
    ft = { 'python' },
    config = function()
      require("conform").setup({
        formatters_by_ft = {
          python = { 'isort', 'black' },
          html = { 'prettier' },
          tera = { 'prettier' }
        },
        formatters = {
          -- NOTE: this will break when used on other filetypes then html and tera.
          prettier = {
            prepend_args = { '--parser', 'html' }
          },
        }
      })
    end,
  },

  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',
      -- Adds path completion
      'hrsh7th/cmp-path',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
  },

  {
    "zbirenbaum/copilot.lua",
    name = "copilot",
    config = function()
      require("copilot").setup({})
    end
  },

  {
    -- Autocompletion for copilot
    "zbirenbaum/copilot-cmp",
    config = function()
      require("copilot_cmp").setup()
    end
  },

  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
      signcolumn = false,  -- Toggle with `:Gitsigns toggle_signs`
      numhl      = true, -- Toggle with `:Gitsigns toggle_numhl`
      on_attach = function(bufnr)
        vim.keymap.set('n', '<leader>hp', require('gitsigns').preview_hunk, { buffer = bufnr, desc = 'Preview git hunk' })

        -- don't override the built-in and fugitive keymaps
        local gs = package.loaded.gitsigns
        vim.keymap.set({ 'n', 'v' }, ']c', function()
          if vim.wo.diff then return ']c' end
          vim.schedule(function() gs.next_hunk() end)
          return '<Ignore>'
        end, { expr = true, buffer = bufnr, desc = "Jump to next hunk" })
        vim.keymap.set({ 'n', 'v' }, '[c', function()
          if vim.wo.diff then return '[c' end
          vim.schedule(function() gs.prev_hunk() end)
          return '<Ignore>'
        end, { expr = true, buffer = bufnr, desc = "Jump to previous hunk" })
      end,
    },
  },

  {
    "Tsuzat/NeoSolarized.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("NeoSolarized").setup {
        style = "dark",
        transparent = true,
        terminal_colors = true,
        enable_italics = true,
        styles = {
          comments = { italic = true },
          keywords = {},
          functions = { bold = true },
          variables = {},
          string = {},
          underline = false,
          undercurl = false,
        },
      }
      vim.cmd [[ colorscheme NeoSolarized ]]
    end
  },

  {
    -- Mode line
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        icons_enabled = true,
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  {
    'lukas-reineke/indent-blankline.nvim',
    main = "ibl",
    opts = {
      scope = { enabled = false }, },
  },

  -- "gc" to comment visual regions/lines
  { 'numToStr/Comment.nvim',  opts = {} },

  {
    'aserowy/tmux.nvim',
    config = function()
      return require('tmux').setup()
    end
  },

  -- surround text objects with keystrokes
  {
      "kylechui/nvim-surround",
      version = "*", -- Use for stability; omit to use `main` branch for the latest features
      event = "VeryLazy",
      config = function()
          require("nvim-surround").setup({})
      end
  },

  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("fzf-lua").setup({})
    end,
    keys = {
      {"<leader>F", ":lua require('fzf-lua').files()<cr>"},
      {"<leader><leader>", ":lua require('fzf-lua').buffers()<cr>"},
      {"<leader>f", ":lua require('fzf-lua').git_files()<cr>"},
      {"<leader>sw", ":lua require('fzf-lua').grep_cword()<cr>"},
      {"<leader>sg", ":lua require('fzf-lua').grep()<cr>"},
      {"<leader>sd", ":lua require('fzf-lua').diagnostics_document()<cr>"},
      {"<leader>sr", ":lua require('fzf-lua').resume()<cr>"},
      {"<leader>ls", ":lua require('fzf-lua').lsp_document_symbols()<cr>"},
      {"<leader>lw", ":lua require('fzf-lua').lsp_workspace_symbols()<cr>"},
      {"gr", ":lua require('fzf-lua').lsp_references()<cr>"},
      {"gI", ":lua require('fzf-lua').lsp_implementations()<cr>"}
    }
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ':TSUpdate',
  },

  -- Import all plugin configurations from the './lua/custom/plugins/' directory.
  { import = 'custom.plugins' },
}, {})

-- [[ Setting options ]]
-- Set highlight on search
vim.o.hlsearch = false
-- Make line numbers default
vim.wo.number = true
-- Enable mouse mode
vim.o.mouse = 'a'
-- Sync clipboard between OS and Neovim.
vim.o.clipboard = 'unnamedplus'
-- Enable break indent
vim.o.breakindent = true
-- Save undo history
vim.o.undofile = true
-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true
-- Keep signcolumn on by default
vim.wo.signcolumn = 'no'
-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300
-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'
-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true
-- Scroll offset: do not move cursor to the bottom of the of the screen
vim.o.scrolloff = 5
-- relative linenumbers
vim.wo.relativenumber = true

-- [[ Basic Keymaps ]]
-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
      'c',
      'go',
      'lua',
      'python',
      'rust',
      'tsx',
      'javascript',
      'svelte',
      'typescript',
      'vimdoc',
      'vim'
    },

    sync_install = true,
    ignore_install = {},
    modules = {},

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = false,

    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = '<c-space>',
        node_incremental = '<c-space>',
        scope_incremental = '<c-s>',
        node_decremental = '<M-space>',
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ['<leader>a'] = '@parameter.inner',
        },
        swap_previous = {
          ['<leader>A'] = '@parameter.inner',
        },
      },
    },
  }
end, 0)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<M-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Format document using `=` or the :Format keyword
  -- nmap('=', vim.lsp.buf.format, 'Format document')
  -- vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
  --   vim.lsp.buf.format()
  -- end, { desc = 'Format current buffer with LSP' })
end


vim.keymap.set('n', '<leader>=', require("conform").format, { desc = 'Format buffer' })

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
-- and add it as a capabilities.
local lspconfig = require('lspconfig')

-- [[ setup lsp servers ]]
-- Golang:
lspconfig.gopls.setup(
  {
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
    on_attach = on_attach,
  }
)

local html_capabilities = vim.lsp.protocol.make_client_capabilities()
html_capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.html.setup(
  {
    capabilities = html_capabilities,
    on_attach = on_attach,
  }
)

lspconfig.pyright.setup(
  {
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
    on_attach = on_attach,
  }
)

-- Rust:
lspconfig.rust_analyzer.setup(
  {
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
    on_attach = on_attach,
  }
)

-- Lua:
lspconfig.lua_ls.setup({
  settings = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  capabilities = require('cmp_nvim_lsp').default_capabilities(),
  on_attach = on_attach,
})

lspconfig.terraformls.setup(
  {
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
    on_attach = on_attach,
  }
)

-- Hide all semantic highlights
for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
  vim.api.nvim_set_hl(0, group, {})
end

-- [[ Configure nvim-cmp ]]
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' }, -- autocomplete from lsp
    { name = "copilot" },  -- copilot for the win
    { name = 'path' },     -- complete paths on disk
    { name = 'luasnip' },  -- snippet completion
  },
}

local tera_group = vim.api.nvim_create_augroup('TeraFileType', { clear = true })
vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  callback = function()
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_option(buf, "filetype", "html")
  end,
  pattern = { '*.html.tera' },
  group = tera_group,
})

-- Make sure terraform files are interpreted as terraform files
local terraform_group = vim.api.nvim_create_augroup('TerraFormFileType', { clear = true })

vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  callback = function()
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_option(buf, "filetype", "terraform")
  end,
  pattern = { '*.tf, *.tfvars', '.terraformrc, terraform.rc' },
  group = terraform_group,
})

vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  callback = function()
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_option(buf, "filetype", "hcl")
  end,
  pattern = { '*.tfstate, *.tfstate.backup' },
  group = terraform_group,
})

-- Auto format imports on save (used for Golang)
function OrganizeImports(wait_ms)
  local params = vim.lsp.util.make_range_params()
  params.context = { only = { "source.organizeImports" } }
  local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, wait_ms)
  for _, res in pairs(result or {}) do
    for _, r in pairs(res.result or {}) do
      if r.edit then
        vim.lsp.util.apply_workspace_edit(r.edit, "UTF-8")
      else
        vim.lsp.buf.execute_command(r.command)
      end
    end
  end
end

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
