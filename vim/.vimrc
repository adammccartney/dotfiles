" .vimrc

" indent recognized filetypes
if has("autocmd")
    filetype indent plugin on
endif

" Rebind <Leader> key
let mapleader=" " 
let maplocalleader=","

" writch buffers in normal mode 
map <Leader>n :bn<cr>
map <Leader>p :bp<cr>
map <Leader>d :bp<cr>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" vim-slime
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "{last}"}

" Atuomatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Better copy & paste
set pastetoggle=<F2>
set clipboard=unnamed

" Mouse and backspace 
set mouse=a " on Debian press ALT and click
set bs=2    " make backspace behave like normal againf

" mouse for traditional vim 
if !has('nvim')
    set ttymouse=xterm2
    set mouse=a
endif


" bind Ctrl+<movement> keys to move around the windows
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" easier moving of code blocks
vnoremap < <gv " better indentation
vnoremap > >gv " better indentation

" Showing line numbers and length
set number " show line numbers
set tw=79  " width of documents (used by gd)
"set nowrap " don't automatically wrap on load
"set fo-=t  " don't automatically wrap text when typing
"set colorcolumn=80
"highlight ColorColumn ctermbg=233


" Easier formatting of paragraphs
vmap Q gq
nmap Q gqap

" Useful settings 
set history=700
set undolevels=700

"No tabs
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab

" Disable stupid backup and swap files - they trigger too many events 
" for file system watchers 
set nobackup
set nowritebackup
set noswapfile

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"=======================================================
"Syntax formatting
"=======================================================

" Lilypond 
filetype off
set runtimepath+=/usr/bin/lilypond
filetype on


" Javascript & Typescript
" JSDocs
let g:javascript_plugin_jsdoc=1
"NGDocs
let g:javascript_plugin_ngdoc=1
" Flow
let g:javascript_plugin_flow=1

"augroup javascript_folding
"    au!
"    au FileType javascript setlocal foldmethod=syntax
"augroup END

" Typescript
let g:typescript_indent_disable=1

" Compiler
let g:typescript_compiler_binars = 'tsc'
let g:typescript_compiler_options = ''



"========================================================
" Packages 
"========================================================

" Use vimplug to manage plugins 

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-dispatch'
Plug 'wilsaj/chuck.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'preservim/nerdtree'
Plug 'supercollider/scvim'
Plug 'leafgarland/typescript-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'bfrg/vim-cpp-modern'
Plug 'junegunn/vim-easy-align'
Plug 'alx741/vim-hindent'
Plug 'pangloss/vim-javascript'
Plug 'digitaltoad/vim-pug'
Plug 'jpalardy/vim-slime'
Plug 'dhruvasagar/vim-table-mode'
Plug 'editorconfig/editorconfig-vim'
Plug 'dbeniamine/cheat.sh-vim'
Plug 'rust-lang/rust.vim'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'
Plug 'ryanoasis/vim-devicons'

if has("nvim")
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
    Plug 'mfussenegger/nvim-lint'
    Plug 'folke/zen-mode.nvim'
    Plug 'EdenEast/nightfox.nvim'
    Plug 'Olical/conjure'
    Plug 'ray-x/go.nvim'
    Plug 'ray-x/guihua.lua', { 'do': 'cd lua/fzy && make'}
    Plug 'github/copilot.vim'
    Plug 'mfussenegger/nvim-dap'
endif

call plug#end()

" =============================================================================
" special config for any vim plugins that got installed
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']


" =============================================================================
" extra config for vim plugins that have some neovim goodies
set completeopt=menu,menuone,noselect


" =============================================================================
" color 
syntax on
if has('gui_running')
    set guicurser+=a:blinkon0
    colorscheme darkblue
elseif has('win32')
    colorscheme slate
elseif has('nvim')
    colorscheme nightfox 
else
    colorscheme ron 
end 
if &term =~ "256color" || &term =~"xterm"
    let &t_SI = "\<Esc>[6 q"
    let &t_EU = "\<Esc>[2 q"
    if exists("&t_SR")
        let &t_SR = "\<Esc>[4 q"
    end
end
if has("x11")
    let &guifont="Noto Mono 11"
elseif has("gui_win32")
    let &guifont="Lucida Console:h11"
end



" Supercollider
" =============
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd set filetype=supercollider
au Filetype supercollider packadd scvim
let g:scFlash = 1

" Haskell
" ========
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords 

" hindent
let g:hindent_on_save = 1
let g:hindent_indent_size = 2
let g:hindent_line_length = 100
let g:hindent_command = "stack exec -- hindent"


" Java
" ====
let g:syntastic_java_checkers = []
let g:EclimFileTypeValidate = 0

" Python 
" ======
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'


" cht.sh: the most useful thing in the universe
" =============================================
" Vim command used to open new buffer
let g:CheatSheetReaderCmd='new"'

" Cheat sheet file type
let g:CheatSheetFt='markdown'

" Program used to retrieve cheat sheet with its arguments
let g:CheatSheetUrlGetter='curl --silent'

" Flag to add cookie file to the query
let g:CheatSheetUrlGetterIdFlag='-b'

" cheat sheet base url
let g:CheatSheetBaseUrl='https://cht.sh'

" cheat sheet settings do not include style settings neiter comments,
" see other options below
let g:CheatSheetUrlSettings='q'

" cheat sheet pager
let g:CheatPager='less -R'

" pygmentize theme used for pager output, see :CheatPager :styles-demo
let g:CheatSheetPagerStyle='rrt'

" Show comments in answers by default
" (setting this to 0 means giving ?Q to the server)
let g:CheatSheetShowCommentsByDefault=1

" Stay in origin buffer (set to 0 to keep focus on the cheat sheet buffer)
let g:CheatSheetStayInOrigBuf=1

" cheat sheet buffer name
let g:CheatSheetBufferName="_cheat"

" Default selection in normal mode (line for whole line, word for word under cursor)
let g:CheatSheetDefaultSelection="line"

" Default query mode
" 0 => buffer
" 1 => replace (do not use or you might loose some lines of code)
" 2 => pager
" 3 => paste after query
" 4 => paste before query
let g:CheatSheetDefaultMode=0

" Path to cheat sheet cookie
let g:CheatSheetIdPath=expand('~/.cht.sh/id')

" Make plugin silent by  setting bellow variable to 1
let g:CheatSheetSilent=0

filetype off
set runtimepath+=/usr/share/lilypond/2.22.0/vim
filetype on
syntax on


if has ('nvim')
    " some setup for plugins
    " nvim-cmp setup
    set completeopt=menu,menuone,noselect   
    " telescope setup
    nnoremap <silent> ff <Cmd>Telescope find_files<CR>
    nnoremap <silent> fg <Cmd>Telescope live_grep<CR>
    nnoremap <silent> fb <Cmd>Telescope buffers<CR>
    nnoremap <silent> fh <Cmd>Telescope help_tags<CR>
endif

" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'
" airline symbols                                                                                                                              
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:WebDevIconsUnicodeDecorateFolderNodeDefaultSymbol = ''

let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {}
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['nerdtree'] = ''


" nvim-terminal
" allows us to use escape key in terminal mode
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
    highlight! link TermCursor Cursor
    highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
endif
