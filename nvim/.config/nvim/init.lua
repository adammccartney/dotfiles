-- init.lua 
-- entrypoint for nvim configuration
vim.cmd([[
set runtimepath^=~/.vim
set runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
]])

-- go setup
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.go",
    callback = function()
      require("go.format").goimport()
    end,
    group = format_sync_grp,
})

--enable plugin
require('go').setup()


--Fuzzy find for stuff
-- fzf-lua setup
vim.api.nvim_set_keymap(
    'n',
    'ff',
    "<cmd>lua require('fzf-lua').files()<CR>",
    { noremap = true, silent = true }
)

vim.api.nvim_set_keymap(
    'n',
    'fg',
    "<cmd>lua require('fzf-lua').grep()<CR>",
    { noremap = true, silent = true }
)

vim.api.nvim_set_keymap(
    'n',
    'fk',
    "<cmd>lua require('fzf-lua').()<CR>",
    { noremap = true, silent = true }
)

--Test function for testing plugins
vim.api.nvim_create_user_command("Test", function()
    package.loaded.refactor = nil
    require('refactor').echo_visual()
end, {})


-- terraform setup 
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = {"*.tf", "*tfvars"},
  callback = function()
    vim.lsp.buf.format()
  end,
  group = format_sync_grp,
})
