-- init.lua
-- entrypoint for nvim configuration

if vim.loader then
    vim.loader.enable()
end

require('adam')

-- General options
local config = vim.fn.stdpath("config")
local xdg_state_dir = vim.fn.stdpath("state")

vim.opt.backup = false -- no backups before writing
vim.opt.backupcopy = 'yes' -- overwrite files instead of renaming + rewriting
vim.opt.backupdir = { xdg_state_dir .. '/backup//' } -- keep backups from creating a tangle for git
vim.opt.completeopt = 'menu' -- show completion menu (for nvim-cmp)
vim.opt.completeopt = vim.opt.completeopt + 'menuone' -- show menu even if there is only one candidate (for nvim-cmp)
vim.opt.completeopt = vim.opt.completeopt + 'noselect' -- don't automatically select canditate (for nvim-cmp)
vim.opt.directory = { xdg_state_dir .. '/swap//', '.' } -- keep swap files out of the way, fallback to cwd on failure

vim.opt.foldlevelstart = 99  -- default unfolded
vim.opt.foldmethod = 'expr'

-------------------------------------------
-- Globals --------------------------------
-------------------------------------------
vim.g.CommandTPreferredImplementation = 'lua'
vim.g.clipboard = 'osc52'
vim.g.mapleader = '<Space>'
vim.g.maplocalleader = ','


vim.cmd('set runtimepath+=~/src/github.com/adammccartney/nvim-oxherd')
-- plugins
if vim.o.loadplugins then
    adam.plugin.load('command-t')
    adam.plugin.load('fzf-lua')
    adam.plugin.load('LuaSnip')
    adam.plugin.load('nvim-treesitter')
    adam.plugin.load('nightfox')
    adam.plugin.load('nvim-oxherd')
    adam.plugin.load('vim-markdown')
    adam.plugin.load('vim-slime')
    adam.plugin.load('vim-easy-align')
    --adam.plugin.load('go.nvim')
    --adam.plugin.load('guihua.lua')
    ---- optional for icon support
    --adam.plugin.load('nvim-web-devicons')
    --adam.plugin.load('render-markdown')
end


-- go setup
--enable plugin


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
})


-------------------------------------
--- LSP -----------------------------
-------------------------------------


----------------
-- popup borders
----------------
vim.cmd [[autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]]
vim.cmd [[autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]]

local _border = 'rounded'

vim.lsp.config('*', {
  capabilities = {
    textDocument = {
      semanticTokens = {
        multilineTokenSupport = true,
      }
    }
  },
  root_markers = { '.git' },
})

vim.lsp.enable({
    "ansiblels",
    "clangd",
    "gopls",
    "lua-language-server",
    "pyright",
    "terraformls",
    "tflint",
})

-----------------------------
--- Diagnostics -------------
-----------------------------

vim.diagnostic.config({
    --virtual_text = { current_line = true },
    float = { border = _border },
})

vim.keymap.set('n', ',gK', vim.diagnostic.open_float)

-----------------------------
--- Autocompletion ----------
-----------------------------
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = _border })
      vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = _border })
    end
  end,
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
--local has_luasnip, luasnip = pcall(require, "luasnip")
--
--if has_luasnip then
--  luasnip.config.set_config {
--      -- remember to keep around last snippet
--  history = true,
--
--      -- dynamic snippets that update as you type
--      -- default is InsertLeave
--      updateevents = 'TextChanged,TextChangedI',
--  }
--  --vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<CR>")
--end



