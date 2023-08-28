return {
	{
		"mfussenegger/nvim-dap",
	},
	{
		"mfussenegger/nvim-dap-python",
		config = function()
			require("dap-python").setup("/usr/bin/python3")
      require('dap-python').test_runner = 'pytest'
		end,
		keys = {
			-- nnoremap <silent> <leader>dn :lua require('dap-python').test_method()<CR>
			-- nnoremap <silent> <leader>df :lua require('dap-python').test_class()<CR>
			-- vnoremap <silent> <leader>ds <ESC>:lua require('dap-python').debug_selection()<CR>
			{
				"<leader>dm",
				function()
					require("dap-python").test_method()
				end,
				desc = "debug test method",
			},
		},
	},
}
