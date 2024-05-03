-- init.lua
-- entrypoint for nvim configuration
vim.cmd([[
set runtimepath^=~/.vim
set runtimepath+=~/.vim/after
" add custom plugin folder
set runtimepath+=~/Code/plugins
let &packpath = &runtimepath
source ~/.vimrc
]])

-- General options
local home = vim.env.HOME
local config = home .. '/.config/nvim'

vim.opt.backup = false -- no backups before writing 
vim.opt.backupcopy = 'yes' -- overwrite files instead of renaming + rewriting 
vim.opt.backupdir = config .. '/backup//' -- keep backups from creating a tangle for git
vim.opt.completeopt = 'menu' -- show completion menu (for nvim-cmp)
vim.opt.completeopt = vim.opt.completeopt + 'menuone' -- show menu even if there is only one candidate (for nvim-cmp)
vim.opt.completeopt = vim.opt.completeopt + 'noselect' -- don't automatically select canditate (for nvim-cmp)
vim.opt.directory = config .. '/nvim/swap//' -- keep swap files out of the way
vim.opt.directory = vim.opt.directory + '.' -- fallback

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

-- Pretty print vim things
P = function(...)
    local args = {}
    for _, arg in ipairs({...}) do
        table.insert(args, vim.inspect(arg))
    end
    print(unpack(args))
    return ...
end
