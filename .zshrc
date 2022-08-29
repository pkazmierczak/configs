export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$HOME/go/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git autojump fzf kubectl virtualenv)

source $ZSH/oh-my-zsh.sh

export LANG="en_US.UTF-8"
export LC_ALL=$LANG

alias vim="nvim"
alias vi="nvim"
export EDITOR="nvim"

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

alias vpy='pip install black mypy flake8 python-lsp-server python-lsp-black'

function cd() {
  if [[ -d ./venv ]] ; then
    deactivate
  fi

  builtin cd $1

  if [[ -d ./venv ]] ; then
    . ./venv/bin/activate
  fi
}
alias tf="terraform"
