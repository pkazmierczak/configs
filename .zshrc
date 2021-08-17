export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$HOME/go/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git autojump fzf kubectl)

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
# BEGIN CW-CLI MANAGED BLOCK
if [ -f /Users/p.kazmierczak/src/cw-cli/path.zsh.inc ] ; then source /Users/p.kazmierczak/src/cw-cli/path.zsh.inc ; fi # cw-cli
# END CW-CLI MANAGED BLOCK
export PATH="/usr/local/opt/ruby/bin:$PATH"


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"


alias tilt='/usr/local/bin/tilt'


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc' ]; then . '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'; fi
