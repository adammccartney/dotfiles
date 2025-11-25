-- lua/adam/keymaps.lua

local ok, adam_lsp = pcall(require, "adam.lsp")

if not ok then
    vim.notify("Could not load adam.lsp: " .. adam_lsp, vim.log.levels.ERROR)
    return
end

local M = {}

function M.setup()
    -- Example / test
    -- vim.keymap.set("n", "<leader>tt", function()
    --     print("keymaps loaded")
    -- end, { desc = "Test keymap" })
    --
    -- lsp
    vim.keymap.set("n", "<leader>lf", function()
        adam_lsp.format()
    end, { desc = "lsp format" })
    vim.keymap.set("n", "<leader>ld", function()
        vim.lsp.buf.definition()
    end, { desc = "lsp definition" })


    -- diagnostics
    vim.keymap.set('n', '<localleader>dK', vim.diagnostic.open_float)
    vim.keymap.set('n', '<localleader>dQ', vim.diagnostic.setqflist)
end

return M
