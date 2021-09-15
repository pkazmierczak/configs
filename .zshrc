export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$HOME/go/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git autojump fzf kubectl virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

export LANG="en_US.UTF-8"
export LC_ALL=$LANG

alias vim="nvim"
alias vi="nvim"

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
    "(☞ﾟヮﾟ)☞ ┻━┻"
    "┳━┳ ヽ(ಠل͜ಠ)ﾉ"
    "¯\(◉‿◉)/¯"
    "¯\(◉◡◔)/¯"
    "(⊙_◎)"
    "ಠಿ_ಠ"
    "(；¬д¬)"
    "ಡ_ಡ"
    "●_●"
    "ಠ﹏ಠ"
    "( ͡° ͜ʖ ͡°)"
    "༼ つ ◕_◕ ༽つ"
    "(ง •̀_•́)ง"
    " ༎ຶ‿༎ຶ "
    "┏━┓┏━┓┏━┓ ︵ /(^.^/)"
    "┳━┳ ヽ(ಠﻝ͜ಠ)ﾉ"
)
alias c='git commit -a -m "$mcommit[$(( $RANDOM % $#mcommit+1 ))]"'
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
