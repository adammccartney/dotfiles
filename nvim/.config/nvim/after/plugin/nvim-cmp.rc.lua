-- Set up nvim-cmp.
-- thanks to wincent https://github.com/wincent/wincent/blob/main/aspects/nvim/files/.config/nvim/after/plugin/nvim-cmp.lua
local has_cmp, cmp = pcall(require, 'cmp')

if has_cmp then
  local rhs = adam.vim.rhs

  local has_luasnip, luasnip = pcall(require, 'luasnip')

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

  -- Until https://github.com/hrsh7th/nvim-cmp/issues/1716
  -- (cmp.ConfirmBehavior.MatchSuffix) gets implemented, use this local wrapper
  -- to choose between `cmp.ConfirmBehavior.Insert` and
  -- `cmp.ConfirmBehavior.Replace`:
  local confirm = function(entry)
    local behavior = cmp.ConfirmBehavior.Replace
    if entry then
      local completion_item = entry.completion_item
      local newText = ''
      if completion_item.textEdit then
        newText = completion_item.textEdit.newText
      elseif type(completion_item.insertText) == 'string' and completion_item.insertText ~= '' then
        newText = completion_item.insertText
      else
        newText = completion_item.word or completion_item.label or ''
      end

      -- How many characters will be different after the cursor position if we
      -- replace?
      local diff_after = math.max(0, entry.replace_range['end'].character + 1) - entry.context.cursor.col

      -- Does the text that will be replaced after the cursor match the suffix
      -- of the `newText` to be inserted? If not, we should `Insert` instead.
      if entry.context.cursor_after_line:sub(1, diff_after) ~= newText:sub(-diff_after) then
        behavior = cmp.ConfirmBehavior.Insert
      end
    end
    cmp.confirm({ select = true, behavior = behavior })
  end

  cmp.setup({

    formatting = {
        -- See:  https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance
        format = function(entry, vim_item)
            vim_item.menu = ({
                buffer = '[Buffer]',
                nvim_lsp = '[LSP]',
                luasnip = '[LuaSnip]',
                nvim_lua = '[Lua]',
            })[entry.source.name]
            return vim_item
        end,
    },

    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-j>'] = cmp.mapping(select_next_item),
      ['<Down>'] = cmp.mapping(select_next_item),
      ['<C-k>'] = cmp.mapping(select_prev_item),
      ['<Up>'] = cmp.mapping(select_prev_item),

      ['<C-y>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          local entry = cmp.get_selected_entry()
          confirm(entry)
        else
          fallback()
        end
      end, { 'i', 's' }),

      ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif has_luasnip and luasnip.in_snippet() and luasnip.jumpable(-1) then
          --  luasnip.jump(-1)
          elseif in_leading_indent() then
            smart_bs(true) -- true means to dedent
          elseif in_whitespace() then
            smart_bs()
          else
            fallback()
          end
        end, { 'i', 's' }),

      ['<Tab>'] = cmp.mapping(function(_fallback)
          if cmp.visible() then
            -- If there is only one completion candidate, use it.
            local entries = cmp.get_entries()
            if #entries == 1 then
              confirm(entries[1])
            else
              cmp.select_next_item()
            end
          elseif has_luasnip and luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          elseif in_whitespace() then
            smart_tab()
          else
            cmp.complete()
          end
        end, { 'i', 's' }),
      }),

    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        if has_luasnip then
            luasnip.expand(args.body)
        end
      end,
    },

    completion = {
        completeopt = 'menu,menuone,noinsert',
    },

    sources = cmp.config.sources({
      { name = 'luasnip' }, -- For luasnip users.
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
end
