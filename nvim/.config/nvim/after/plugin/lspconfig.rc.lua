-- Global mappings
-- See `:help vim.diagnostic.*` for documentation of the following functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)
vim.keymap.set('n', '<space>r', vim.lsp.buf.references)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function()
    --vim.keymap.set('n', '<Leader>vd', '<cmd>lua vim.diagnostic.open_float()<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', {buffer = true, silent = true })
    vim.keymap.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.declaration()<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<LocalLeader>r', '<cmd>lua vim.lsp.buf.rename()<CR>', { buffer = true, silent = true })
end



local has_lspconfig, lspconfig = pcall(require, 'lspconfig')

if has_lspconfig then
    local capabilities = vim.lsp.protocol.make_client_capabilities()

    local has_cmp_nvim_lsp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
    if has_cmp_nvim_lsp then
       capabilities = cmp_nvim_lsp.default_capabilities(vim.lsp.protocol.make_client_capabilities())
    end


    --------
    -- setup clangd independently
    -------
    lspconfig.clangd.setup {
        on_attach = on_attach,
        cmd = {
                "clangd",
                "--background-index",
                "--suggest-missing-includes"
            },
            filetypes = {"c", "cpp", "objc", "objcpp"},
    }



    --------
    -- go
    --------
    lspconfig.gopls.setup {
        cmd = {'gopls'},
        -- for postfix snippets and analyzers
            capabilities = capabilities,
            settings = {
              gopls = {
                  experimentalPostfixCompletions = true,
                  analyses = {
                    unusedparams = true,
                    shadow = true,
                 },
                 staticcheck = true,
                },
            },
            on_attach = on_attach,
    }


    ----------------
    -- lua
    ----------------
    local runtime_path = vim.split(package.path, ";")
    table.insert(runtime_path, "lua/?.lua")
    table.insert(runtime_path, "lua/?/init.lua")

    lspconfig.lua_ls.setup {
      settings = {
        Lua = {
          runtime = {
            -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
            version = 'LuaJIT',
            path = runtime_path,
          },
          diagnostics = {
            -- Get the language server to recognize the `vim` global
            globals = {'vim'},
          },
          workspace = {
            -- Make the server aware of Neovim runtime files
            library = vim.api.nvim_get_runtime_file("", true),
            checkThirdParty = false,
          },
          -- Do not send telemetry data containing a randomized but unique identifier
          telemetry = {
            enable = false,
          },
        },
      },
    }

    --------------------
    --- python
    --------------------
    lspconfig.pyright.setup{}
end
