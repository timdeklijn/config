
return {
  'mfussenegger/nvim-lint',
  ft = { 'python' },
  config = function()
    require('lint').linters_by_ft = {
      -- TODO: I should add some functionality where this can be 
      --       switched based on project/client
      python = { 'pylint', 'mypy' },
    }
  end
}
