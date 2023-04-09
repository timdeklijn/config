return {

	-- color scheme
  {
    'Mofiqul/dracula.nvim',
    name = "dracula",
    lazy = false,
    priority = 1000,
    config = function ()
      local dracula = require("dracula")
      dracula.setup({
        show_end_of_buffer = true,
        transparent_bg = false,
        italic_comment = false,
      })
      vim.cmd [[ colorscheme dracula ]]
    end
  },

	-- nicer modeline
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "auto",
				component_separators = "|",
				section_separators = "",
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
    "github/copilot.vim",
    name = "copilot",
    event = "InsertEnter",
    lazy = true,
  },
}