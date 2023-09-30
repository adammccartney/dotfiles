" .vimrc

if v:progname == 'vi'
  set noloadplugins
endif

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

" Javascript & Typescript
" JSDocs
let g:javascript_plugin_jsdoc=1
"NGDocs
let g:javascript_plugin_ngdoc=1
" Flow
let g:javascript_plugin_flow=1

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
    colorscheme desert
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

" Python
" ======
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'
