# Per-command profiling:

#zmodload zsh/datetime
#setopt promptsubst
#PS4='+$EPOCHREALTIME %N:%i> '
#exec 3>&2 2> startlog.$$
#setopt xtrace prompt_subst

# Per-function profiling:

#zmodload zsh/zprof

# editing
bindkey -e # emacs bindings

#[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

fpath=(~/.zsh $fpath)

# history
export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

# options
setopt SHARE_HISTORY           # share history across shells
setopt NO_FLOW_CONTROL         # disable start (C-s) and stop (C-q) characters
setopt NO_HIST_IGNORE_ALL_DUPS # don't filter non-contiguous duplicates from history
setopt HIST_FIND_NO_DUPS       # don't show dupes when searching
setopt HIST_IGNORE_DUPS        # do filter contiguous duplicates from history
setopt HIST_IGNORE_SPACE       # [default] don't record commands starting with a space
setopt HIST_VERIFY             # confirm history expansion (!$, !!, !foo)
setopt PRINT_EXIT_VALUE        # [default] for non-zero exit status

# Completions
#
fpath=($HOME/.zsh/completions $fpath)

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/adam/.zshrc'

# End of lines added by compinstall
autoload -Uz compinit && compinit

# Set the prompt
autoload -U colors
colors

# [username@hostname dir] $
PROMPT='%F{blue}%B%C%f %F{yellow}%#%b%f '

# Git setup (right prompt)
autoload -Uz vcs_info

zstyle ':vcs_info:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{green}◼%f"
zstyle ':vcs_info:*' unstagedstr "%F{red}◼%f"
zstyle ':vcs_info:git:*' formats '(%b%m%c%u)'
zstyle ':vcs_info:git:*' actionformats '(%b|%a%m%c%u)'

precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst

RPROMPT='${vcs_info_msg_0_} %~'

# pull in other files
source $HOME/.zsh/aliases

# End profiling (uncomment when necessary)
#

# Per-command profiling:

#unsetopt xtrace
#exec 2>&3 3>&-

# Per-function profiling:

# zprof
#zprof > /tmp/foo
