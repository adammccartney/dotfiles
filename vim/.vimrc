" .vimrc

if v:progname == 'vi'
  set noloadplugins
endif

" indent recognized filetypes
if has("autocmd")
    filetype indent plugin on
endif

" Rebind <Leader> key
let mapleader="\<Space>"
let maplocalleader=","

" switch buffers in normal mode
map <Leader>n :bn<cr>
map <Leader>p :bp<cr>
map <Leader>d :bp<cr>

" switch tabs
map <LocalLeader>n :tabn<cr>
map <LocalLeader>p :tabp<cr>


" Start interactive EasyAlign in visual mode (e.g. vipga)
"xmap ga <Plug>(EasyAlign)

" vim-slime
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "{last}"}

" Atuomatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Better copy & paste
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

set laststatus=2
set encoding=utf-8
set autoindent
set magic
set nowrap
set ignorecase
set number " show line numbers
set tw=80  " width of documents (used by gd)

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

"=======================================================
"Syntax formatting
"=======================================================

" Lilypond 
filetype off
set runtimepath+=/usr/bin/lilypond
filetype on
syntax on


"========================================================
" Packages
"========================================================

" only use packages if running nvim, see init.lua

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
    colorscheme default
else
    colorscheme desert
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

" Python
" ======
let g:python_host_prog = '~/.virtualenvs/pynvim/bin/python'
let g:python3_host_prog = '~/.virtualenvs/pynvim/bin/python3'

autocmd FileType python compiler flake8


" Javascript & Typescript
" =======================
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

" FZF default command
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'

" non-breaing space is farsi n
set list listchars=tab:▸\ ,trail:·,extends:»,precedes:«,nbsp:ن

" Insert footers for mail
nnoremap ,jmail :-1read $HOME/.vim/snippets/mails/jaas.mail.template<CR>2j
" Insert a k8s deployment yaml
nnoremap ,akd :-1read $HOME/.vim/snippets/k8s/deployment.yaml<CR>
nnoremap ,akpv :-1read $HOME/.vim/snippets/k8s/pv.yaml<CR>
" Python snippers
nnoremap ,aplg :-1read $HOME/.vim/snippets/py/logger.py<CR>
nnoremap ,apb :-1read $HOME/.vim/snippets/py/breakpoint.py<CR>1j
