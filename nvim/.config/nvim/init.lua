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


if vim.loader then
    vim.loader.enable()
end

require('adam')


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

-------------------------------------------
-- Globals --------------------------------
-------------------------------------------
vim.g.CommandTPreferredImplementation = 'lua'

-- plugins
if vim.o.loadplugins then
    vim.cmd('packadd! LuaSnip')
    vim.cmd('packadd! nvim-lspconfig')
    vim.cmd('packadd! nvim-treesitter')
    vim.cmd('packadd! nvim-cmp')
    vim.cmd('packadd! cmp-nvim-lsp')
    vim.cmd('packadd! cmp-nvim-lua')
    vim.cmd('packadd! cmp_luasnip')
    vim.cmd('packadd! nightfox')
    vim.cmd('packadd! fzf-lua')
    vim.cmd('packadd! vim-slime')
    --vim.cmd('packadd! conjure')
    --vim.cmd('packadd! go.nvim')
    --vim.cmd('packadd! guihua.lua')
    ---- optional for icon support
    --vim.cmd('packadd! nvim-web-devicons')
    vim.cmd('packadd! command-t')
end


-- go setup
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.go",
    callback = function()
      require("go.format").goimport()
    end,
    group = format_sync_grp,
})

--enable plugin
--require('go').setup()


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


-- luasnip config
local has_luasnip, luasnip = pcall(require, "luasnip")

if has_luasnip then
  luasnip.config.set_config {
      -- remember to keep around last snippet
      history = true,

      -- dynamic snippets that update as you type
      -- default is InsertLeave
      updateevents = 'TextChanged,TextChangedI',
  }
  --vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<CR>")
end



