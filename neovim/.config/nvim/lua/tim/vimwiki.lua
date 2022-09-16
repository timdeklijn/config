-- Add some shortcuts for vimwiki

-- Custom function that will go create a diary not for the current day. Then go
-- to the diary index and generate all links to the different diary entries.
function diary_start()
  vim.cmd[[
    VimwikiMakeDiaryNote
    VimwikiDiaryIndex
    VimwikiDiaryGenerateLinks
  ]]
end

-- Map diary_start to `<leader>W`.
vim.api.nvim_set_keymap(
  'n',
  '<leader>W',
  [[<cmd>lua diary_start()<CR>]],
  { noremap=true, silent=true }
)
