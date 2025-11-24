set -o vi
alias ls='ls --color=auto --group-directories-first'
export EDITOR=vim
export DIRENV_LOG_FORMAT=""
bindkey -v
bindkey '^R' history-incremental-search-backward

