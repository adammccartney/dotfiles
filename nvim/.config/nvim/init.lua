-- init.lua 
-- entrypoint for nvim configuration
vim.cmd([[
set runtimepath^=~/.vim
set runtimepath+=~/.vim/after
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
vim.opt.cursorline = true -- highlight current line
vim.opt.diffopt = vim.opt.diffopt + 'foldcolumn:0' -- don't show fold column in diff view
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

-- scnvim setup 
--
local scnvim = require 'scnvim'
local map = scnvim.map
local map_expr = scnvim.map_expr
scnvim.setup {
  keymaps = {
    ['<M-e>'] = map('editor.send_line', {'i', 'n'}),
    ['<C-e>'] = {
      map('editor.send_block', {'i', 'n'}),
      map('editor.send_selection', 'x'),
    },
    ['<CR>'] = map('postwin.toggle'),
    ['<M-CR>'] = map('postwin.toggle', 'i'),
    ['<M-L>'] = map('postwin.clear', {'n', 'i'}),
    ['<C-k>'] = map('signature.show', {'n', 'i'}),
    ['<F12>'] = map('sclang.hard_stop', {'n', 'x', 'i'}),
    ['<leader>st'] = map('sclang.start'),
    ['<leader>sk'] = map('sclang.recompile'),
    ['<F1>'] = map_expr('s.boot'),
    ['<F2>'] = map_expr('s.meter'),
  },
  editor = {
    highlight = {
      color = 'IncSearch',
    },
  },
  postwin = {
    float = {
      enabled = true,
    },
  },
}

-- Conjure guile setup 
vim.cmd([[ 
let g:conjure#filetype#scheme = "conjure.client.guile.socket" 
]])
