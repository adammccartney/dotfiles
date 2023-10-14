
# Set the prompt
# [username@hostname dir] $
PROMPT='[%n@%m %C]$'
RPROMPT='%t'

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

# editing
bindkey -e # emacs bindings

# pull in other files
source $HOME/.zsh/aliases
