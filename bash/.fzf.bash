# Setup fzf
# ---------
if [[ ! "$PATH" == "*/$HOME/.local/src/fzf/bin*" ]]; then
  PATH="${PATH:+${PATH}:}$HOME/.local/src/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$HOME/.local/src/fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "$HOME/.local/src/fzf/shell/key-bindings.bash"
