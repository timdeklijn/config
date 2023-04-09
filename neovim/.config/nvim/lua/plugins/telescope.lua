return {
	{
		"nvim-telescope/telescope.nvim",
		cmd = "Telescope",
		version = false, -- telescope did only one release, so use HEAD for now
		dependencies = {
		 	"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
			config = function()
				require("telescope").load_extension("fzf")
			end,
		},
		keys = {
			{
				"<leader><leader>",
				function()
					require("telescope.builtin").buffers()
				end,
				desc = "List all buffers",
			},
			{
				"<leader>f",
				function()
					require("telescope.builtin").find_files({})
				end,
				desc = "Find files",
			},
			{
				"<leader>sg",
				function()
					require("telescope.builtin").live_grep({})
				end,
				desc = "Find files",
			},
			{
				"<leader>ss",
				function()
					require("telescope.builtin").lsp_document_symbols({})
				end,
				desc = "LSP Document Symbols",
			},
			{
				"<leader>sS",
				function()
					require("telescope.builtin").lsp_workspace_symbols({})
				end,
				desc = "LSP Workspace Symbols",
			},
			{
				"<leader>sR",
				"<cmd>Telescope resume<cr>",
				desc = "Resume",
			},
		},
		opts = {
			defaults = {
				prompt_prefix = " ",
				selection_caret = " ",
				file_ignore_patterns = { ".git" },
				mappings = {
					i = {
						["<C-u>"] = false,
						["<C-d>"] = false,
					},
				},
			},
			pickers = {
				find_files = {
					hidden = true,
				},
				buffers = {
					ignore_current_buffer = true,
					sort_lastused = true,
				},
			},
		},
	},
}
