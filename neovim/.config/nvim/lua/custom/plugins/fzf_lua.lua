-- Fuzzy finder plugin. Search all the things!

return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = 'VimEnter',
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
}
