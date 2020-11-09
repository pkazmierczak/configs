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
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
# BEGIN CW-CLI MANAGED BLOCK
if [ -f /Users/p.kazmierczak/src/cw-cli/path.zsh.inc ] ; then source /Users/p.kazmierczak/src/cw-cli/path.zsh.inc ; fi # cw-cli
# END CW-CLI MANAGED BLOCK

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/p.kazmierczak/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/p.kazmierczak/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/p.kazmierczak/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/p.kazmierczak/google-cloud-sdk/completion.zsh.inc'; fi
