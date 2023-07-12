local dap = require("dap")

local js_based_languages = {"typescript", "javascript", "javascriptreact", "typescriptreact"}

for _, language in ipairs(js_based_languages) do
    dap.configurations[language] = {
        {
            type = "pwa-node",
            request = "launch",
            name = "Launch file",
            program = "${file}",
            cwd = "${workspaceFolder}",
        },
        {
            type = "pwa-node",
            request = "attach",
            name = "Attach to Process",
            processId = require 'dap.utils'.pick_process,
            cwd = '${workspaceFolder}',
        },
        {
            type = "pwa-chrome",
            request = "launch",
            name = "Launch Chrome With \"localhost\"",
            url = "http://localhost:3000",
            webRoot = "${workspaceFolder}",
            userDataDir = "${workspaceFolder}/.vscode/chrome",
        }
    }
end
