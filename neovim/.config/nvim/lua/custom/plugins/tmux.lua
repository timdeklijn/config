-- Plugin for tmux integration:
--
-- Navigate through panes using (simplified) vim keys:
--   <C-h> - left
--   <C-j> - down
--   <C-k> - up
--   <C-l> - right
return {
  'aserowy/tmux.nvim',
  config = function()
    return require('tmux').setup()
  end
}
