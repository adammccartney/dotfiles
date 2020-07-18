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

"=======================================================
" Virtual Environments
"======================================================
let g:virtualenv_directory = $VIRTUAL_ENV

"=======================================================
"Syntax formatting
"=======================================================

" Lilypond 
filetype off
set runtimepath+=/usr/local/share/lilypond/current/vim/
filetype on
syntax on

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

" Setup Vim-LaTex
" cd ~/.vim/bundle && git clone https://github.com/vim-latex/vim-latex.git
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
set shellslash

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'



"
" t pope's fugitive ( Git wrapper for vim )
" mkdir -p ~/.vim/bundle/tpope/start
" cd ~/.vim/pack/tpope/start
" git clone https://tpope.io/vim/fugitive.git
" vim -u NONE -c "helptags fugitive/doc" -c q
"
"

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


"==============================================================================
"You Complete Me 
"==============================================================================

let g:ycm_clangd_binary_path = "/bin/clangd-10"


" Jupyter Notebook integration
" cd ~/.vim/bundle
" git clone https://github.com/broesler/jupyter-vim.git
"
"Always use the same virtual env for vim, regardless of what Python 
" environment is loaded in the shell from which vim is launched
"let g:vim_virtualenv_path = '~/.virtualenvs/frog'
"if exists('g:vim_virtualenv_path')
"    pythonx import os; import vim
"    pythonx activate_this = os.path.join(vim.eval('g:vim_virtualenv_path'), 'bin/activate.py')
"    pythonx with open(activate_this) as f: exec(f.read(), {'__file__': activate_this})
"endif
"


"=================
" highlightedyank
"================

let g:highlightedyank_highlight_duration = 250

