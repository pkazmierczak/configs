# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/piotr/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/piotr/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/piotr/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/Users/piotr/.fzf/shell/key-bindings.bash"

