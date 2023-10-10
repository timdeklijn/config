return {
	"nvim-neo-tree/neo-tree.nvim",
	branch = "v3.x",
	lazy = false,
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
		"MunifTanjim/nui.nvim",
	},
	config = function ()
	    vim.keymap.set('n', '<C-n>', "<cmd>Neotree toggle reveal right<cr>", { desc = 'Neotree' })
	end
}
