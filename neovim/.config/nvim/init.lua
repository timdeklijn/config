-- examples:
-- - https://github.com/LazyVim/LazyVim/tree/main/lua/lazyvim/plugins

-- =============================================================================
-- Bootsrap Lazy nvim
-- =============================================================================

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- =============================================================================
-- Basic config
-- =============================================================================


-- sane defaults
vim.o.hlsearch = false
vim.wo.number = true
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.updatetime = 250
-- vim.wo.signcolumn = 'no'
vim.o.cursorline = false
vim.o.completeopt = 'menuone,noselect'

vim.g.mapleader = " "
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- =============================================================================
-- autocommands
-- =============================================================================

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Create autocommands to show cursorlines in active windows/buffers, but
-- hide them when leaving the window.
vim.api.nvim_create_autocmd(
  { "VimEnter", "WinEnter", "BufWinEnter" },
  { command = "setlocal cursorline" }
)
vim.api.nvim_create_autocmd(
  "WinLeave",
  { command = "setlocal nocursorline" }
)

vim.cmd([[silent! autocmd! filetypedetect BufRead,BufNewFile *.tf]])
vim.cmd([[autocmd BufRead,BufNewFile *.hcl set filetype=hcl]])
vim.cmd([[autocmd BufRead,BufNewFile .terraformrc,terraform.rc set filetype=hcl]])
vim.cmd([[autocmd BufRead,BufNewFile *.tf,*.tfvars set filetype=terraform]])
vim.cmd([[autocmd BufRead,BufNewFile *.tfstate,*.tfstate.backup set filetype=json]])

-- Use system clipboard
vim.api.nvim_set_option("clipboard", "unnamed")

-- Load additional config
require("lazy").setup("plugins")
