case $- in
  *i*) ;;
    *) return;;
esac

export OSH='/Users/piotr/.oh-my-bash'

OSH_THEME="minimal"
OMB_USE_SUDO=true

completions=(
  git
  composer
  ssh
)

aliases=(
  general
)

export LANG="en_US.UTF-8"
export LC_ALL=$LANG

alias vim="nvim"
alias vi="nvim"
export EDITOR="nvim"

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

plugins=(
  fzf
  git
  bashmarks
)

source "$OSH"/oh-my-bash.sh

