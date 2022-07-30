nnoremap <silent> ff <Cmd>Telescope find_files<CR>
nnoremap <silent> fg <Cmd>Telescope live_grep<CR>
nnoremap <silent> fb <Cmd>Telescope buffers<CR>
nnoremap <silent> fh <Cmd>Telescope help_tags<CR>

lua <<EOF
local actions = require('telescope.actions')

require('telescope').setup {
    defaults = {
        mappings = {
            n = {
                ["q"] = actions.close
                },
            },
        }
    }
EOF
