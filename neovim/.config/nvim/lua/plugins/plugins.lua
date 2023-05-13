return {

	-- color scheme
	{
		"rebelot/kanagawa.nvim",
		name = "kanagawa",
		lazy = false,
		priority = 1000,
		config = function()
			require("kanagawa").setup({
				options = {
					compile = true, -- enable compiling the colorscheme
					undercurl = true, -- enable undercurls
					commentStyle = { italic = false },
					functionStyle = { bold = true },
					keywordStyle = { italic = false },
					statementStyle = { bold = true },
					typeStyle = { bold = true },
					transparent = false, -- do not set background color
					dimInactive = true, -- dim inactive window `:h hl-NormalNC`
					terminalColors = false, -- define vim.g.terminal_color_{0,17}
				},
        -- use this to make sure the gutter background is the same as
        -- the text background.
				colors = {
					theme = {
						all = {
							ui = {
								bg_gutter = "none",
							},
						},
					},
				},
			})

			vim.cmd([[ colorscheme kanagawa-wave ]])
		end,
	},

	-- nicer modeline
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "auto",
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
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
