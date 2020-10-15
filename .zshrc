export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$HOME/go/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git autojump fzf)

source $ZSH/oh-my-zsh.sh

export LANG="en_US.UTF-8"
export LC_ALL=$LANG

alias vim="nvim"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

mcommit=(
    "¯\\_(ツ)_/¯"
    "¯\\_㋡_/¯"
    "(╯°Д°)╯︵/(.□ . \\)"
    "(┛◉Д◉)┛彡┻━┻"
    "┻━┻︵ \\(°□°)/ ︵ ┻━┻"
    "(┛ಠ_ಠ)┛彡┻━┻"
    "(ノಠ益ಠ)ノ彡┻━┻"
    "(╯°□°)╯︵ ┻━┻"
    "(˚Õ˚)ر ~~~~╚╩╩╝"
    "ヽ(ຈل͜ຈ)ﾉ︵ ┻━┻"
    "┬─┬ノ( º _ ºノ)"
)
alias c='git commit -a -m "$mcommit[$(( $RANDOM % $#mcommit+1 ))]"'
[ -f "/Users/piotr/.ghcup/env" ] && source "/Users/piotr/.ghcup/env" # ghcup-env
