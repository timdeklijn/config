return {
	{
		"kylechui/nvim-surround",
		opts = {},
	},

	-- "gc" to comment visual regions/lines
	{
		"numToStr/Comment.nvim",
		name = "Comment.nvim",
		config = function()
			require("Comment").setup({})
		end,
	},

}
