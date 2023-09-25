return {

  {
    "ellisonleao/gruvbox.nvim",
    name = "gruvbox",
    lazy = false,
    priority = 1000,
    config = function()
      require("gruvbox").setup({
        terminal_colors = true, -- add neovim terminal colors
        undercurl = true,
        underline = true,
        bold = true,
        italic = {
          strings = true,
          emphasis = true,
          comments = true,
          operators = false,
          folds = true,
        },
        strikethrough = true,
        invert_selection = false,
        invert_signs = false,
        invert_tabline = false,
        invert_intend_guides = false,
        inverse = true, -- invert background for search, diffs, statuslines and errors
        contrast = "hard", -- can be "hard", "soft" or empty string
        palette_overrides = {},
        overrides = {},
        dim_inactive = false,
        transparent_mode = false,
      })
      vim.cmd("colorscheme gruvbox")
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
