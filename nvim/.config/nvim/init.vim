set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
lua << EOF
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

-- guihua defaults

  -- default mapping
  maps = {
    close_view = '<C-e>',
    send_qf = '<C-q>',
    save = '<C-s>',
    jump_to_list = '<C-w>k',
    jump_to_preview = '<C-w>j',
    prev = '<C-p>',
    next = '<C-n>',
    pageup = '<C-b>',
    pagedown = '<C-f>',
    confirm = '<C-o>',
    split = '<C-s>',
    vsplit = '<C-v>',
  }

  --
  require('guihua.maps').setup({
  maps = {
    close_view = '<C-x>',
  }
})

  -- fzf-lua setup
  vim.api.nvim_set_keymap('n', 'ff',
  "<cmd>lua require('fzf-lua').files()<CR>",
  { noremap = true, silent = true })
  vim.api.nvim_set_keymap('n', 'fg',
  "<cmd>lua require('fzf-lua').grep()<CR>",
  { noremap = true, silent = true })

EOF
