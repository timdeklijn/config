return {

	-- color scheme
	{
		"bluz71/vim-moonfly-colors",
		name = "moonfly",
		lazy = false,
		priority = 1000,
    config = function()
      vim.g.moonflyCursorColor = true
      vim.g.moonflyItalics = false
      vim.cmd [[ colorscheme moonfly ]]
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
			require("gitsigns").setup()
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
