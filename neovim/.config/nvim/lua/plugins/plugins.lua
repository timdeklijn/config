return {

	-- color scheme
  {
    "sainnhe/gruvbox-material",
    name="gruvbox-material",
    priority = 1000,
    lazy = false,
    config = function()
      vim.cmd([[
        let g:gruvbox_material_background = 'hard'
        let g:gruvbox_material_foreground = 'material'
        let g:gruvbox_material_enable_bold = 1
        let g:gruvbox_material_dim_inactive_windows = 1
        let g:gruvbox_material_diagnostic_virtual_text = 'colored'
        let g:gruvbox_material_better_performance = 1
        colorscheme gruvbox-material
      ]])
    end,
  },

	-- nicer modeline
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = true,
				theme = "auto",
			},
		},
	},
	-- Integrate nicely with tmux
	{
		"christoomey/vim-tmux-navigator",
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
