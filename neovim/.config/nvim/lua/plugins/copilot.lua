-- Coptilot tho help programming. The current setup works ok by plugging it into
-- cmp. Remember to complete the copitlot suggestions with ENTER and not TAB to keep the indent level.
--
-- TODO:
--    - keybind to turn copilot on and off
--    - ignore certain files

return {
  {
    "zbirenbaum/copilot.lua",
    name = "copilot",
    event = "InsertEnter",
    lazy = true,
    config = function()
      require("copilot").setup({
        suggestion = { enabled = false },
        panel = { enabled = false },
        filetypes = {
          markdown = false,
          vimwiki = false,
        }
      })
    end,
  },

  {
    "zbirenbaum/copilot-cmp",
    after = { "copilot.lua" },
    event = "InsertEnter",
    config = function ()
      require("copilot_cmp").setup()
    end
  }
}
