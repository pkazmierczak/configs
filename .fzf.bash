# Setup fzf
# ---------
if [[ ! "$PATH" == */home/piotr/.fzf/bin* ]]; then
  export PATH="$PATH:/home/piotr/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/piotr/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/piotr/.fzf/shell/key-bindings.bash"

