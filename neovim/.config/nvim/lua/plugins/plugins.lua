return {

	-- color scheme
  {
    "EdenEast/nightfox.nvim",
    name = "nightfox",
    lazy = false,
    priority = 1000,
    options = {
      compile_path = vim.fn.stdpath("cache") .. "/nightfox",
      compile_file_suffix = "_compiled",
      transparent = false,
      terminal_colors = true,
      dim_inactive = true,
      module_default = true,
      styles = {
        comments = "NONE",
        conditionals = "bold",
        constants = "bold",
        functions = "bold",
        keywords = "bold",
        numbers = "NONE",
        operators = "NONE",
        strings = "NONE",
        types = "bold",
        variables = "NONE",
      },
    },
    config = function ()
      vim.cmd [[ colorscheme nightfox ]]
    end
  },

	-- nicer modeline
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "auto",
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
			},
		},
	},
	-- Integrate nicely with tmux
	{
		"christoomey/vim-tmux-navigator",
	},

	-- "gc" to comment visual regions/lines
	{
		"numToStr/Comment.nvim",
		name = "Comment.nvim",
		config = function()
			require("Comment").setup({})
		end,
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
      { "<leader>gg", ":Git<CR>", desc = "Open Fugitive" },
      { "<leader>gp", ":Git push<CR>", desc = "Open Fugitive" },
    },
  },

}
