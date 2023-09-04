return {

  {
    "bluz71/vim-nightfly-colors",
    name = "nightfly",
    lazy = false,
    priority = 1000,
    config = function()
      vim.g.nightflyCursorColor = true
      vim.g.nightflyUnderlineMatchParen = true
      vim.g.nightflyVirtualTextColor = true
      vim.cmd [[ colorscheme nightfly ]]
    end
  },

  -- nicer modeline
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        icons_enabled = false,
        theme = "auto",
      },
    },
  },

  -- File tree
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      git = {
        enable = true,
        ignore = false,
        timeout = 400,
      },
      view = {
        side = "right",
      },
    },
    keys = {
      { "<C-n>", ":NvimTreeToggle<CR>", desc = "Neotree" },
    },
  },

  -- library used by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },

  -- Show git changes in the sign column
  {
    "lewis6991/gitsigns.nvim",
    event = "BufEnter",
    config = function()
      require("gitsigns").setup({
        signcolumn = false,
        numhl = true,
      })
    end,
  },

  {
    "tpope/vim-fugitive",
    name = "figutive",
    keys = {
      { "<leader>gg", ":Git<CR>",      desc = "Open Fugitive" },
      { "<leader>gp", ":Git push<CR>", desc = "Open Fugitive" },
    },
  },
}
