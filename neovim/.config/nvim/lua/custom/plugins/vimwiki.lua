return {
  "vimwiki/vimwiki",
  lazy = false,
  config = function ()
    vim.cmd[[ 
      let g:vimwiki_list = [{'path': '~/Dropbox/notes/vimwiki/', 'syntax': 'markdown', 'ext': 'md'}] 
    ]]
  end
}
