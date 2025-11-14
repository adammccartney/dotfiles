-- Syntax folding
-- setlocal foldmethod=indent
vim.api.nvim_set_option_value("foldmethod", "indent", { scope = "local" , win = 0 })

vim.g.python3_host_prog = os.getenv("HOME") .. "/.virtualenvs/pyd/bin/python"
