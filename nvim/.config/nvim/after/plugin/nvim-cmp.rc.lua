-- Set up nvim-cmp.
local cmp = require'cmp'

-- thanks to wincent https://github.com/wincent/wincent/blob/main/aspects/nvim/files/.config/nvim/after/plugin/nvim-cmp.lua
local rhs = adam.vim.rhs

-- Returns the current column number.
local column = function()
  local _line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col
end

-- Returns true if the cursor is in leftmost column or at a whitespace
-- character.
local in_whitespace = function()
  local col = column()
  return col == 0 or vim.api.nvim_get_current_line():sub(col, col):match('%s')
end

local in_leading_indent = function()
  local col = column()
  local line = vim.api.nvim_get_current_line()
  local prefix = line:sub(1, col)
  return prefix:find('^%s*$')
end

local shift_width = function()
  if vim.o.softtabstop <= 0 then
    return vim.fn.shiftwidth()
  else
    return vim.o.softtabstop
  end
end


-- Complement to `smart_tab()`.
--
-- When 'noexpandtab' is set (ie. hard tabs are in use), backspace:
--
--    - On the left (ie. in the indent) will delete a tab.
--    - On the right (when in trailing whitespace) will delete enough
--      spaces to get back to the previous tabstop.
--    - Everywhere else it will just delete the previous character.
--
-- For other buffers ('expandtab'), we let Neovim behave as standard and that
-- yields intuitive behavior, unless the `dedent` parameter is truthy, in
-- which case we issue <C-D> to dedent (see `:help i_CTRL-D`).
local smart_bs = function(dedent)
  local keys = nil
  if vim.o.expandtab then
    if dedent then
      keys = rhs('<C-D>')
    else
      keys = rhs('<BS>')
    end
  else
    local col = column()
    local line = vim.api.nvim_get_current_line()
    local prefix = line:sub(1, col)
    if in_leading_indent() then
      keys = rhs('<BS>')
    else
      local previous_char = prefix:sub(#prefix, #prefix)
      if previous_char ~= ' ' then
        keys = rhs('<BS>')
      else
        -- Delete enough spaces to take us back to the previous tabstop.
        --
        -- Originally I was calculating the number of <BS> to send, but
        -- Neovim has some special casing that causes one <BS> to delete
        -- multiple characters even when 'expandtab' is off (eg. if you hit
        -- <BS> after pressing <CR> on a line with trailing whitespace and
        -- Neovim inserts whitespace to match.
        --
        -- So, turn 'expandtab' on temporarily and let Neovim figure out
        -- what a single <BS> should do.
        --
        -- See `:h i_CTRL-\_CTRL-O`.
        keys = rhs('<C-\\><C-o>:set expandtab<CR><BS><C-\\><C-o>:set noexpandtab<CR>')
      end
    end
  end
  vim.api.nvim_feedkeys(keys, 'nt', true)
end
-- In buffers where 'noexpandtab' is set (ie. hard tabs are in use), <Tab>:
--
--    - Inserts a tab on the left (for indentation).
--    - Inserts spaces everywhere else (for alignment).
--
-- For other buffers (ie. where 'expandtab' applies), we use spaces everywhere.
local smart_tab = function(opts)
  local keys = nil
  if vim.o.expandtab then
    keys = '<Tab>' -- Neovim will insert spaces.
  else
    local col = column()
    local line = vim.api.nvim_get_current_line()
    local prefix = line:sub(1, col)
    local in_leading_indent = prefix:find('^%s*$')
    if in_leading_indent then
      keys = '<Tab>' -- Neovim will insert a hard tab.
    else
      -- virtcol() returns last column occupied, so if cursor is on a
      -- tab it will report `actual column + tabstop` instead of `actual
      -- column`. So, get last column of previous character instead, and
      -- add 1 to it.
      local sw = shift_width()
      local previous_char = prefix:sub(#prefix, #prefix)
      local previous_column = #prefix - #previous_char + 1
      local current_column = vim.fn.virtcol({ vim.fn.line('.'), previous_column }) + 1
      local remainder = (current_column - 1) % sw
      local move = remainder == 0 and sw or sw - remainder
      keys = (' '):rep(move)
    end
  end

  vim.api.nvim_feedkeys(rhs(keys), 'nt', true)
end


local select_next_item = function(fallback)
    if cmp.visible() then
      cmp.select_next_item()
    else
      fallback()
    end
end

local select_prev_item = function(fallback)
    if cmp.visible() then
      cmp.select_prev_item()
    else
      fallback()
    end
end

cmp.setup({

  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-j>'] = cmp.mapping(select_next_item),
    ['<Down>'] = cmp.mapping(select_next_item),
    ['<C-k>'] = cmp.mapping(select_prev_item),
    ['<Up>'] = cmp.mapping(select_prev_item),
    ['<C-y>'] = cmp.mapping(cmp.get_selected_entry),
    ['<S-Tab>'] = cmp.select_prev_item(),
    ['<Tab>'] = cmp.complete(),
  }),

  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },

  sources = cmp.config.sources({
    -- { name = 'luasnip' }, -- For luasnip users.
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'buffer' },
    { name = 'calc' },
    { name = 'path' },
    { name = 'emoji' },
  }),

  window = {
    completion = cmp.config.window.bordered({
        border = 'single',
        col_offset = -1,
        scrollbar = false,
        scrolloff = 3,
        -- Default for bordered() is 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None'
        -- Default for non-bordered, which we'll use here, is:
        winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None',
    }),
    documentation = cmp.config.window.bordered({
            border = 'solid',
        scrollbar = false,
        -- Default for bordered() is 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None'
        -- Default for non-bordered is 'FloatBorder:NormalFloat'
        -- Suggestion from: https://github.com/hrsh7th/nvim-cmp/issues/2042
        -- is to use 'NormalFloat:NormalFloat,FloatBorder:FloatBorder,CursorLine:Visual,Search:None'
        -- but this also seems to suffice:
        winhighlight = 'CursorLine:Visual,Search:None',
    }),
  },

})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'git' }, -- You can specify the `git` source if [you were installed it](https://github.com/petertriho/cmp-git).
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Set up lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()
-- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
require('lspconfig')['pyright'].setup {
  capabilities = capabilities
}

require('lspconfig')['ts_ls'].setup {
  capabilities = capabilities
}

require('lspconfig')['terraformls'].setup {
  capabilities = capabilities
}
