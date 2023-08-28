-- inspired by: https://codeberg.org/j4de/nvim/src/branch/master/lua/plugins/lsp.lua
return {
  -- nvim-lspconfig
  {
    'neovim/nvim-lspconfig',
    keys = {
      {
        'gd',
        function()
          return require('telescope.builtin').lsp_definitions()
        end,
        desc = 'Goto Definition',
      },
      {
        'gr',
        function()
          return require('telescope.builtin').lsp_references()
        end,
        desc = 'References',
      },
      {
        'gD',
        vim.lsp.buf.declaration,
        desc = 'Goto Declaration',
      },
      {
        'gI',
        function()
          return require('telescope.builtin').lsp_implementations()
        end,
        desc = 'Goto Implementation',
      },
      {
        'gy',
        function()
          return require('telescope.builtin').lsp_type_definitions()
        end,
        desc = 'Goto T[y]pe Definition',
      },
      {
        'K',
        vim.lsp.buf.hover,
        desc = 'Hover',
      },
      {
        'gK',
        vim.lsp.buf.signature_help,
        desc = 'Signature Help',
      },
      {
        '<leader>=', 
        vim.lsp.buf.format,
        desc = 'Format',
      },
    },
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local lspconfig = require('lspconfig')
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
      lspconfig.pylsp.setup({ capabilities = capabilities })
      lspconfig.lua_ls.setup({ capabilities = capabilities })
      lspconfig.marksman.setup({ capabilities = capabilities })
    end,
  },

  -- fidget.nvim
  {
    'j-hui/fidget.nvim',
    event = 'LspAttach',
    tag = 'legacy',
    opts = { window = { blend = 0 } },
  },

  -- actions-preview.nvim
  {
    'aznhe21/actions-preview.nvim',
    dependencies = 'nvim-telescope/telescope.nvim',
    opts = {
      telescope = {
        sorting_strategy = 'ascending',
        layout_strategy = 'vertical',
        layout_config = {
          width = 0.8,
          height = 0.9,
          prompt_position = 'top',
          preview_cutoff = 20,
          preview_height = function(_, _, max_lines)
            return max_lines - 15
          end,
        },
      },
    },
    keys = {
      {
        'gf',
        function()
          return require('actions-preview').code_actions()
        end,
        desc = 'Open actions-preview.nvim',
      },
    },
  },

  -- nvim-lightbulb
  { 'kosayoda/nvim-lightbulb', event = 'LspAttach', opts = { autocmd = { enabled = true } } },

  -- nvim-dap
  {
    'mfussenegger/nvim-dap',
    dependencies = {
      -- Virtual text for nvim-dap
      { 'theHamsta/nvim-dap-virtual-text', config = true },

      -- UI for nvim-dap
      {
        'rcarriga/nvim-dap-ui',
        keys = {
          {
            '<leader>du',
            function()
              return require('dapui').toggle()
            end,
            desc = 'Dap UI',
          },
          {
            '<leader>de',
            function()
              return require('dapui').eval()
            end,
            desc = 'Eval',
            mode = { 'n', 'v' },
          },
        },
        config = function()
          local dap = require('dap')
          local dapui = require('dapui')
          dapui.setup()
          dap.listeners.after.event_initialized['dapui_config'] = function()
            dapui.open()
          end
          dap.listeners.before.event_terminated['dapui_config'] = function()
            dapui.close()
          end
          dap.listeners.before.event_exited['dapui_config'] = function()
            dapui.close()
          end
        end,
      },
    },
    keys = {
      {
        '<leader>dB',
        function()
          return require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))
        end,
        desc = 'Breakpoint Condition',
      },
      {
        '<leader>db',
        function()
          return require('dap').toggle_breakpoint()
        end,
        desc = 'Toggle Breakpoint',
      },
      {
        '<leader>dc',
        function()
          return require('dap').continue()
        end,
        desc = 'Continue',
      },
      {
        '<leader>dC',
        function()
          return require('dap').run_to_cursor()
        end,
        desc = 'Run to Cursor',
      },
      {
        '<leader>dg',
        function()
          return require('dap').goto_()
        end,
        desc = 'Go to line (no execute)',
      },
      {
        '<leader>di',
        function()
          return require('dap').step_into()
        end,
        desc = 'Step Into',
      },
      {
        '<leader>dj',
        function()
          return require('dap').down()
        end,
        desc = 'Down',
      },
      {
        '<leader>dk',
        function()
          return require('dap').up()
        end,
        desc = 'Up',
      },
      {
        '<leader>dl',
        function()
          return require('dap').run_last()
        end,
        desc = 'Run Last',
      },
      {
        '<leader>do',
        function()
          return require('dap').step_out()
        end,
        desc = 'Step Out',
      },
      {
        '<leader>dO',
        function()
          return require('dap').step_over()
        end,
        desc = 'Step Over',
      },
      {
        '<leader>dp',
        function()
          return require('dap').pause()
        end,
        desc = 'Pause',
      },
      {
        '<leader>dr',
        function()
          return require('dap').repl.toggle()
        end,
        desc = 'Toggle REPL',
      },
      {
        '<leader>ds',
        function()
          return require('dap').session()
        end,
        desc = 'Session',
      },
      {
        '<leader>dt',
        function()
          return require('dap').terminate()
        end,
        desc = 'Terminate',
      },
      {
        '<leader>dw',
        function()
          return require('dap.ui.widgets').hover()
        end,
        desc = 'Widgets',
      },
    },
    config = function()
      vim.api.nvim_set_hl(0, 'DapStoppedLine', { default = true, link = 'Visual' })
    end,
  },
}


-- -- LSP Server Settings
-- local servers = {
--   jsonls = {},
--   lua_ls = {
--       Lua = {
--         workspace = {
--           checkThirdParty = false,
--         },
--         diagnostics = {
--           globals = { 'vim' },
--         },
--         completion = {
--           callSnippet = "Replace",
--         },
--       },
--   },
--   pylsp = {},
--   gopls = {},
--   rust_analyzer = {},
--   dockerls = {},
--   bashls = {},
--   yamlls = {
--       yaml = {
--         keyOrdering = false
--       }
--   },
--   terraformls = {},
--   marksman = {},
--   zls = {},
-- }
-- 
-- -- Global mappings.
-- -- See `:help vim.diagnostic.*` for documentation on any of the below functions
-- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
-- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
-- vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
-- vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)
-- 
-- -- [[ LSP ]]
-- -- Use LspAttach autocommand to only map the following keys
-- -- after the language server attaches to the current buffer
-- vim.api.nvim_create_autocmd('LspAttach', {
--   group = vim.api.nvim_create_augroup('UserLspConfig', {}),
--   callback = function(ev)
-- 
--     -- Buffer local mappings.
--     -- See `:help vim.lsp.*` for documentation on any of the below functions
--     local opts = { buffer = ev.buf }
--     vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
--     vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
--     vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
--     vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
--     vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
--     vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
--     vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
--     vim.keymap.set('n', '<space>wl', function()
--       print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
--     end, opts)
--     vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
--     vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
--     vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
--     vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
--     vim.keymap.set('n', '<space>f', function()
--       vim.lsp.buf.format { async = true }
--     end, opts)
--   end,
-- })
-- 
-- -- Create a command `:Format` local to the LSP buffer
-- vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
--   vim.lsp.buf.format()
-- end, { desc = 'Format current buffer with LSP' })
-- 
-- -- Bind format to a keymap
-- vim.keymap.set('n', '<leader>=', ':Format<CR>', opts)
-- 
-- -- prevent lsp coloring
-- client.server_capabilities.semanticTokensProvider = nil
-- 
-- -- Auto format imports on save (used for Golang)
-- function OrganizeImports(wait_ms)
--   local params = vim.lsp.util.make_range_params()
--   params.context = { only = { "source.organizeImports" } }
--   local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, wait_ms)
--   for _, res in pairs(result or {}) do
--     for _, r in pairs(res.result or {}) do
--       if r.edit then
--         vim.lsp.util.apply_workspace_edit(r.edit, "UTF-8")
--       else
--         vim.lsp.buf.execute_command(r.command)
--       end
--     end
--   end
-- end
-- 
-- return {
--   -- lspconfig
--   {
--     "neovim/nvim-lspconfig",
--     dependencies = {
--       "hrsh7th/cmp-nvim-lsp"
--     },
--     capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
--     opts = {
--       -- options for vim.diagnostic.config()
--       diagnostics = {
--         underline = true,
--         update_in_insert = false,
--         virtual_text = { spacing = 4, prefix = "●" },
--         severity_sort = true,
--       },
--       -- Automatically format on save
--       autoformat = true,
--       format = {
--         formatting_options = nil,
--         timeout_ms = nil,
--       },
--     },
--     config = function()
--       for _, lsp in ipairs(servers) do
--         lspconfig[lsp].setup {
--         capabilities = capabilities,
--         options = opts,
--         }
--       end
--     end,
--   },
-- 
--   "simrat39/inlay-hints.nvim",
--   "j-hui/fidget.nvim",
--   "jose-elias-alvarez/nvim-lsp-ts-utils",
--   "hrsh7th/cmp-nvim-lsp",
-- 
--   {
--     'ray-x/lsp_signature.nvim',
--     opts = { floating_window = true }
--   },
-- 
-- }
