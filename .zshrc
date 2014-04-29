autoload -U compinit
compinit -C

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' \
      'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

. ~/.liquidprompt/liquidprompt

