# fpath=( "$HOME/.zfunctions" $fpath )
# autoload -U promptinit; promptinit
# PURE_GIT_PULL=0
# prompt pure
# 
# MENU_COMPLETE=1
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

LEIN_FAST_TRAMPOLINE=y
export LEIN_FAST_TRAMPOLINE
alias cljsbuild="lein trampoline cljsbuild $@"

# GOPATH=$HOME/devel/go-workspace
# export GOPATH

EDITOR=/usr/local/bin/vim
export EDITOR

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"  # This loads RVM
# Customize to your needs...
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/texlive/2015/bin/x86_64-darwin:/Users/piotr/.rvm/bin:/Users/piotr/Library/haskell/bin:/opt/puppetlabs/bin
export PATH=$PATH:$GOPATH/bin

alias glg="git log --graph --oneline --decorate --date=relative --all"
alias gst="git status"
alias close="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
alias lock="open /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app"

function mfa () {
   oathtool --base32 --totp "$(cat ~/.aws/$1.mfa)" ;
 }

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
