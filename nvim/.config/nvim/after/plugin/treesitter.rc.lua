local has_tsconfigs, tsconfigs = pcall(require, 'nvim-treesitter.configs')


if has_tsconfigs then
	tsconfigs.setup {
	    highlight = {
		enable = true,
		disable = {},
		},
	    indent = {
		enable = false,
		disable = {},
		},
	    ensure_installed = {
		"tsx",
		"yaml",
		"json",
		"html",
		"javascript",
		"typescript",
		"css",
		"python",
		"go",
		"c",
		"lua"
		},

	    ignore_install = { "supercolider" },
	    playground = {
	      enable = true,
	      disable = {},
	      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
	      persist_queries = false, -- Whether the query persists across vim sessions
	      keybindings = {
	        toggle_query_editor = 'o',
	        toggle_hl_groups = 'i',
	        toggle_injected_languages = 't',
	        toggle_anonymous_nodes = 'a',
	        toggle_language_display = 'I',
	        focus_language = 'f',
	        unfocus_language = 'F',
	        update = 'R',
	        goto_node = '<cr>',
	        show_help = '?',
	      },
	    }
    }
    end

local has_tsparsers, tsparsers = pcall(require, "nvim-treesitter.parsers")

if has_tsparsers then
	local parser_config = tsparsers.get_parser_configs()
	parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
end
