" .vimrc


" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Atuomatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Better copy & paste
set pastetoggle=<F2>
set clipboard=unnamed

" Mouse and backspace 
set mouse=a " on DEbian press ALT and click
set bs=2    " make backspace behave like normal againf

" mouse 
set ttymouse=xterm2
set mouse=a

" Rebind <Leader> key
let mapleader="," 

" Easier moving between tabs
map <Leader>n <esc>:tabprevious<CR>
map <Leader>m <esc>:tabnext<CR>

" bind Ctrl+<movement> keys to move around the windows
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" map sort function to a key
vnoremap <Leader>s :sort<CR>

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

" Enable snytax highlighting 
" You need to reload this file after installing this
filetype off
filetype plugin indent on
syntax on

" Showing line numbers and length
set number " show line numbers
set tw=79  " width of documents (used by gd)
set nowrap " don't automatically wrap on load
set fo-=t  " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=233


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
syntax on


" Javascript & Typescript
" JSDocs
let g:javascript_plugin_jsdoc=1
"NGDocs
let g:javascript_plugin_ngdoc=1
" Flow
let g:javascript_plugin_flow=1

augroup javascript_folding
    au!
    au FileType javascript setlocal foldmethod=syntax
augroup END

" Typescript
"
let g:typescript_indent_disable=1

" Compiler
let g:typescript_compiler_binars = 'tsc'
let g:typescript_compiler_options = ''



"========================================================
" Packages 
"========================================================


" Setup Pathogen to manage your plugins 
" mkdir -p ~/.vim/autoload ~/.vim/bundle
" curl -so ~/.vim/autoload/pathogen.vim
" https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
" Now you can install any plugin into a .vim/bundle/plugin-name/ folder
call pathogen#infect()
syntax on 
filetype plugin indent on


" =====================================================
" Python IDE-setup
" =====================================================

set pyxversion=3

" Settings for vim-powerline
" cd ~/.vim/bundle
" git clone git://github.com/Lokaltog/vim-powerline.git
set laststatus=2

" Settings for ctrlp
" cd ~/.vim/bundle
" git clone https://github.com/kien/ctrlp.vim.git
let g:ctrlp_max_height = 30
set wildignore+=*.pyc
set wildignore+=*_build/*
set wildignore+=*/coverage/*

" Settings for jedi-vim
" cd ~/.vim/bundle
" git clone git://github.com/davidhalter/jedi-vim.git
let g:jedi#usages_command = "<leader>z"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
map <Leader>b Oimport ipdb; ipdb.set_trace() #BREAKPOINT<C-c>

" Python folding
" mkdir -p ~/.vim/ftplugin
" wget -O ~/.vim/ftplugin/python_editing.vim
" http://www.vim.org/scripts/download_script.php?src_id=5494
set nofoldenable


