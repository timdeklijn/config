-- <Leader>ww -- Open default wiki index file.
-- <Leader>w<leader>w -- dairy file
-- <Leader>wt -- Open default wiki index file in a new tab.
-- <Leader>ws -- Select and open wiki index file.
-- <Leader>wd -- Delete wiki file you are in.
-- <Leader>wr -- Rename wiki file you are in.
-- <Enter> -- Follow/Create wiki link.
-- <Shift-Enter> -- Split and follow/create wiki link.
-- <Ctrl-Enter> -- Vertical split and follow/create wiki link.
-- <Backspace> -- Go back to parent(previous) wiki link.
-- <Tab> -- Find next wiki link.
-- <Shift-Tab> -- Find previous wiki link.

return {
	{
		"vimwiki/vimwiki",
		name = "vimwiki",
		lazy = true,
		keys = { "<leader>ww" },
		init = function()
			vim.g.vimwiki_list = {
				{
					path = "~/TimDocs/vimwiki",
					syntax = "markdown",
					ext = ".md",
				},
			}
		end,
	},
}
