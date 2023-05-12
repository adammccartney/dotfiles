local configs = require('lspconfig/configs')
local util = require('lspconfig/util')

configs.ansiblels = {
    default_config = {
        cmd = { "ansible-language-server" },
        filetypes = { "ansible.yaml" },
        root_dir = util.path.dirname,
    },
    docs = {
        description = [[ 
Custom config for ansible language server 
]],
        default_config = {
            root_dir = [[root_pattern(".git")]],
        },
    },
}
