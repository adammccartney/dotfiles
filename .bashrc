# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# point to a populated terminfo database and explicitly set term
export TERMINFO=/usr/share/terminfo
export TERM=tmux-256color

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# Custom prompt that displays the time in upper left corner
# along with a shortened path name on the left hand side, 
# i.e. it only displays the name of the topmost path dir

PS1="\[\033[s\033[0;0H\033[0;49m\033[K\033[1;33m\t\033[u\]<\u@\h \W>\$"


# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1="<\u@\h \W>\$"
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


# Function definitions.
if [ -f ~/.bash_functions ]; then
  . ~/.bash_functions
fi

if [ -d "$HOME/bin" ] ; then
    PATH="HOME/bin:$PATH"
fi

# tmp var
if [ -f ~/.bashvar ] ; then
    . ~/.bashvar
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash


# set up environment variables for compiling from source in $HOME/.local
export PATH=$HOME/.local/bin:$PATH
export C_INCLUDE_PATH=$HOME/.local/include
export CPLUS_INCLUDE_PATH=$HOME/.local/include
export LIBRARY_PATH=$HOME/.local/lib
export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig

# This tells the run time linker where to find
# files installed in the home directory.
# WARNING: may cause issues if an officially installed package is looking for
# a library that is also installed on the system in a more holy manner.
export LD_LIBRARY_PATH=$HOME/.local/lib

# Set mail environment variable
MAIL=/var/mail/adam && export MAIL

# set realtime
export SOUND_CARD_IRQ=169

export PG_OF_PATH=$HOME/openFrameworks

# GOPATH
export GOPATH=$HOME/gobook

export PATH=$PATH:/sbin:/opt/ghc/bin:/opt/riscv/bin

# temp variable for rehashing blog
export OLDPOSTS=/media/websites/content/music
export CONTENT=$HOME/Websites/admccartney/content
export STATIC=$HOME/Websites/admccartney/static
export NEWSHORTCODES=$HOME/Websites/admccartney/layouts/shortcodes
export NEWSOUNDS=$HOME/Websites/admccartney/static/sounds

source "$HOME/.cargo/env"

# Remote server variable
export REMOTE1=206.189.52.96
. "$HOME/.cargo/env"

#add flutter to path
export PATH="$HOME/.local/src/flutter/bin:$PATH"
export CHROME_EXECUTABLE=/usr/bin/chromium
#add android studio to path
export PATH="$HOME/.local/tools/android-studio/bin:$PATH"

# use vim keybindings
set -o vi
[ -f "/home/adam/.ghcup/env" ] && source "/home/adam/.ghcup/env" # ghcup-env

# git tools
. ~/git-completion.bash
. ~/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\w$(__git_ps1 " (%s)")\$ '

# virtualenv wrapper
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS=' -p /usr/bin/python3 '
export PROJECT_HOME=$HOME/.local/venvs
source /usr/local/bin/virtualenvwrapper.sh

