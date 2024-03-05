return {
  "vimwiki/vimwiki",
  keys = { "<leader>ww", "<leader>wt" },
  -- vimwiki options should be passed in the init function. During the
  -- config is too late to configure the plugin.
  init = function ()
    vim.cmd[[ 
      let g:vimwiki_list = [{'path': '~/Dropbox/notes/vimwiki/', 'syntax': 'markdown', 'ext': 'md'}] 
    ]]
  end,
  config = function ()
  end
}
