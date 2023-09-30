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
vim.g['conjure#extract#tree_sitter#enabled'] = true

-- Mappings 
--vim.keymap.set('n', '<F5>', function() require('dap').continue() end)
--vim.keymap.set('n', '<F10>', function() require('dap').step_over() end)
--vim.keymap.set('n', '<F11>', function() require('dap').step_into() end)
--vim.keymap.set('n', '<F12>', function() require('dap').step_out() end)
--vim.keymap.set('n', '<Leader>b', function() require('dap').toggle_breakpoint() end)
--vim.keymap.set('n', '<Leader>B', function() require('dap').set_breakpoint() end)
--vim.keymap.set('n', '<Leader>lp', function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
--vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
--vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)
--vim.keymap.set({'n', 'v'}, '<Leader>dh', function()
--  require('dap.ui.widgets').hover()
--end)
--vim.keymap.set({'n', 'v'}, '<Leader>dp', function()
--  require('dap.ui.widgets').preview()
--end)
--vim.keymap.set('n', '<Leader>df', function()
--  local widgets = require('dap.ui.widgets')
--  widgets.centered_float(widgets.frames)
--end)
--vim.keymap.set('n', '<Leader>ds', function()
--  local widgets = require('dap.ui.widgets')
--  widgets.centered_float(widgets.scopes)
--end)
--

-- Pretty print vim things
P = function(...)
    local args = {}
    for _, arg in ipairs({...}) do
        table.insert(args, vim.inspect(arg))
    end
    print(unpack(args))
    return ...
end
