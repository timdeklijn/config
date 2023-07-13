return {
	{
		"mfussenegger/nvim-dap",
	},
	{
		"jay-babu/mason-nvim-dap.nvim",
		event = { "BufReadPost", "BufNewFile" },
		config = function()
			require("mason-nvim-dap").setup({
				ensure_installed = { "python" },
				automatic_installation = true,
				handlers = {
					function(config)
						require("mason-nvim-dap").default_setup(config)
					end,
					python = function(config)
						config.adapters = {
							type = "executable",
							command = "/usr/bin/python3",
							args = {
								"-m",
								"debugpy.adapter",
							},
						}
						require("mason-nvim-dap").default_setup(config)
					end,
				},
			})
		end,
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
