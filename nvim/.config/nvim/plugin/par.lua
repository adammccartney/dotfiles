if vim.o.formatprg == '' and vim.fn.executable('par') then
  -- Copied from original source: 
  -- https://github.com/wincent/wincent/blob/main/aspects/nvim/files/.config/nvim/plugin/par.lua
  --
  -- Good luck understanding these options, even after reading the man page:
  --
  -- r: equivalent to "r3" (repeat 3).
  -- T: equivalent to "T8"; tab characters are expanded to 8 spaces.
  -- b: equivalent to "b1"; prefixes/suffixes may not contain trailing/leading body characters.
  -- q: equivalent to "q1"; supply vacant lines between quoting nesting levels.
  -- R: equivalent to "R1"; report errors for words longer than `width - prefix - suffix`.
  -- B=.,?_A_a_0: add to "body" characters periods, commas, question marks, upper and lowercase letters, decimal digits.
  -- Q=_s>|: add to "quote" characters spaces, greater than, vertical bar.
  --
  -- See: https://manpages.debian.org/testing/par/par.1.en.html
  --
  -- > par is necessarily complex. For those who wish to use it
  -- > immediately and understand it later, assign the PARINIT
  -- > environment variable the following value:
  -- >
  -- >     rTbgqR B=.,?_A_a Q=_s>|
  -- >
  -- > The spaces, question mark, greater-than sign, and vertical bar
  -- > will probably have to be escaped or quoted to prevent your shell
  -- > from interpreting them.
  --
  -- Note that you can use `gw`/`gww` if `gq`/`gqq` ever does the wrong thing.
  --
  vim.opt.formatprg = vim.env.HOME .. '/bin/safe-par rTbqR B=.,\\?_A_a_0 Q=_s\\>\\|'
end
