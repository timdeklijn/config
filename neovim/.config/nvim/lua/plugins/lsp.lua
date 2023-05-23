-- LSP Server Settings
local servers = {
  jsonls = {},
  lua_ls = {
      Lua = {
        workspace = {
          checkThirdParty = false,
        },
        diagnostics = {
          globals = { 'vim' },
        },
        completion = {
          callSnippet = "Replace",
        },
      },
  },
  pyright = {},
  gopls = {},
  rust_analyzer = {},
  dockerls = {},
  bashls = {},
  yamlls = {
      yaml = {
        keyOrdering = false
      }
  },
  terraformls = {},
  marksman = {},
  zls = {},
}


-- [[ LSP ]]
local on_attach = function(client, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ls', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
  nmap('<leader>e', vim.diagnostic.open_float, '[E]rror hover')

  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })

  -- Bind format to a keymap
  nmap('<leader>=', ':Format<CR>', 'Format buffer')

  -- prevent lsp coloring
  client.server_capabilities.semanticTokensProvider = nil
end

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

return {

  -- lspconfig
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp"
    },
    config = function()
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

      -- Setup mason so it can manage external tooling
      require('mason').setup()

      -- Ensure the servers above are installed
      local mason_lspconfig = require 'mason-lspconfig'

      mason_lspconfig.setup {
        ensure_installed = vim.tbl_keys(servers),
      }

      mason_lspconfig.setup_handlers {
        function(server_name)
          require('lspconfig')[server_name].setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
          }
        end,
      }
    end,
    opts = {
      -- options for vim.diagnostic.config()
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = { spacing = 4, prefix = "●" },
        severity_sort = true,
      },
      -- Automatically format on save
      autoformat = true,
      format = {
        formatting_options = nil,
        timeout_ms = nil,
      },
    },
  },

  "simrat39/inlay-hints.nvim",
  "j-hui/fidget.nvim",
  "jose-elias-alvarez/nvim-lsp-ts-utils",
  "hrsh7th/cmp-nvim-lsp",

  -- auto install stuff
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    config = function()
      require("mason").setup({})
    end,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "bashls",
          "dockerls",
          "gopls",
          "lua_ls",
          "marksman",
          "pyright",
          "rust_analyzer",
          "terraformls",
          "yamlls",
          "zls",
        },
        automatic_installation = true,
      })
    end,
  },

  -- linters + formatters
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason.nvim" },
    opts = function()
      local nls = require("null-ls")
      nls.setup({
        debounce = 150,
        save_after_format = false,
        sources = {
          nls.builtins.diagnostics.flake8,
          nls.builtins.formatting.black,
          nls.builtins.formatting.isort,
          nls.builtins.formatting.shfmt,
          nls.builtins.diagnostics.markdownlint,
          nls.builtins.formatting.stylua,
        },
        root_dir = require("null-ls.utils").root_pattern("Makefile", ".git"),
      })
    end,
  },

  {
    'ray-x/lsp_signature.nvim',
    opts = { floating_window = true }
  },

}
