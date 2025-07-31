-- lua/adam/lsp.lua
local M = {}

function M.format()
    if vim.lsp.buf.format then
        vim.lsp.buf.format({ async = true })
    else
        vim.lsp.buf.formatting()
    end
end


return M
