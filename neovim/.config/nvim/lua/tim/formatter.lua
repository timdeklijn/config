-- formatter.nvim config
--
-- Tim de Klijn, 2021

require('formatter').setup({
  logging = false,
  filetype = {
    markdown = {
      function()
        return {
          exe = "pandoc",
          args = {"-f", "markdown", "-t", "gfm", "-sp", "--tab-stop=2"},
          stdin = true
        }
      end
    },
    python = {
      function()
        return {
          exe = "black",
          args = {vim.api.nvim_buf_get_name(0), "--line-length", "88", "-q"},
          stdin = false
        }
      end
    }
  }
})

