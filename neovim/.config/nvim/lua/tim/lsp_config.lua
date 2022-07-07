-- LSP settings
--
-- Tim de Klijn, 2021

local nvim_lsp = require('lspconfig')

local on_attach = function(_client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

-- Add lsp to nvim-cmp
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

nvim_lsp.rust_analyzer.setup { 
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities
}

nvim_lsp.tsserver.setup { 
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

nvim_lsp.gopls.setup { 
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

nvim_lsp.pyright.setup { 
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  pycodestyle = { enabled = false },
  capabilities = capabilities 
}

nvim_lsp.bashls.setup { 
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

nvim_lsp.html.setup{ 
  on_attach = on_attach, 
  cmd = {"vscode-html-language-server", "--stdio" }, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

nvim_lsp.julials.setup{
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

nvim_lsp.yamlls.setup{
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

require'lspconfig'.terraformls.setup{
  on_attach = on_attach,
  flags = { debounce_text_changes = 150 },
  capabilities = capabilities
}

require'lspconfig'.svelte.setup{
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}

require'lspconfig'.metals.setup{
  on_attach = on_attach, 
  flags = { debounce_text_changes = 150 }, 
  capabilities = capabilities 
}


require("null-ls").setup({
  sources = {
    require("null-ls").builtins.formatting.black,
    require("null-ls").builtins.diagnostics.flake8,
    require("null-ls").builtins.diagnostics.mypy,
    require("null-ls").builtins.formatting.markdownlint,
    require("null-ls").builtins.formatting.rustfmt,
  },
})

-- Map :Format to vim.lsp.buf.formatting(), also mapped to <SPACE>=
vim.cmd([[ command! Format execute 'lua vim.lsp.buf.formatting()' ]])
