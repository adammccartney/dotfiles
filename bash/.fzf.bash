# Setup fzf
# ---------
if [[ ! "$PATH" == */home/adam/.local/src/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/adam/.local/src/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/adam/.local/src/fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/adam/.local/src/fzf/shell/key-bindings.bash"
