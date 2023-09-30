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
    'fh',
    "<cmd>lua require('fzf-lua').help_tags()<CR>",
    { noremap = true, silent = true }
)
