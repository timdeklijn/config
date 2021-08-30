-- Use markdown syntax for vimwiki
vim.cmd[[ let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}] ]]

-- Only markdown files in vimwiki path are vimwiki files
vim.cmd[[ let g:vimwiki_global_ext = 0 ]]
