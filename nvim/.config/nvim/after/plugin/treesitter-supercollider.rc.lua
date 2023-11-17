
-- tree-sitter-supercollider
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.supercollider = {
	install_info = {
		url = "~/.local/src/tree-sitter-supercollider",
		-- url = "https://github.com/madskjeldgaard/tree-sitter-supercollider",
		files = {"src/parser.c"},
		maintainer = "@madskjeldgaard"
	},
	filetype = "supercollider", -- if filetype does not agrees with parser name
}

