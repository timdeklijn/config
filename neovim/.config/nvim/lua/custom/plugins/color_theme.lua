-- define color theme here

return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function ()
    require("tokyonight").setup({
      style = "storm",
      transparent = true,
      hide_inactive_statusline = true,
     styles = {
        comments = { italic = true },
        keywords = { italic = true },
        functions = { bold = true },
        variables = {},
        sidebars = "transparent",
        floats = "transparent",
      },
    })
    vim.cmd[[ colorscheme tokyonight ]]
  end
}
