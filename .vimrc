" .vimrc

" indent recognized filetypes
if has("autocmd")
    filetype indent plugin on
endif

syntax on

" Rebind <Leader> key
let mapleader="," 

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

"Show whitespace
" MUST be inserted BEFORE the colorscheme command
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
au InsertLeave * match ExtraWhitespace /\s+$/

" Color scheme
" mkdir -p ~/.vim/colors && cd ~/.vim/colors
" wget -= wombat256mod.vim http://www.vim.org/scripts/download_script.php?src_id=13400
set t_Co=256
color wombat256mod

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
Plug 'wilsaj/chuck.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'preservim/nerdtree'
Plug 'supercollider/scvim'
Plug 'leafgarland/typescript-vim'
Plug 'vim-airline/vim-airline'
Plug 'bfrg/vim-cpp-modern'
Plug 'junegunn/vim-easy-align'
Plug 'alx741/vim-hindent'
Plug 'pangloss/vim-javascript'
Plug 'digitaltoad/vim-pug'
Plug 'jpalardy/vim-slime'
Plug 'dhruvasagar/vim-table-mode'

if has("nvim")
    Plug 'neovim/nvim-lspconfig'
    Plug 'glepnir/lspsaga.nvim'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
endif

call plug#end()

" =============================================================================
"

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
