# do nothing if called non-interactively
case $- in
    *i*) ;;
    *) return;;
esac
HISTCONTROL=ignoreboth
HISTSIZE=10000
HISTFILESIZE=20000
shopt -s histappend

# append to history after each command
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
EDITOR=vim

# PS0='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
shopt -s checkwinsize

if [ -x /usr/bin/dircolors ]; then
    use_color=true
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
else
    use_color=false
fi

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases $use_color
fi

if [ -f ~/local/rc/bashrc ]; then
    source ~/local/rc/bashrc $use_color
fi

if [ -f ~/.bash/git-aware-prompt/main.sh ]; then
    export GITAWAREPROMPT=~/.bash/git-aware-prompt
    source "${GITAWAREPROMPT}/main.sh"
    export PS1="\${debian_chroot:+(\$debian_chroot)}\[\033[33;40m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\] \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\n\$ "
fi

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [[ -z "${PYTHONPATH}" ]]; then
    export PYTHONPATH=.:./src:./test
else
    export PYTHONPATH=.:./src:./test:${PYTHONPATH}
fi

# Tab completion for stack
eval "$(stack --bash-completion-script stack)"

## Configuration for pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYTHONPATH=./src:./test
export EDITOR=vim
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
export PATH="$PYENV_ROOT/overrides:$PATH"

## Configuration for AWS
export AWS_CONFIG_FILE=/home/bart/.aws_config
export AWS_VAULT_BACKEND=secret-service
export AWS_REGION=eu-west-1

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start --components=secrets)
    export SSH_AUTH_SOCK
fi

with-aws() {
  AWS_CONFIG=$HOME/.aws.vault/config /opt/bin/aws-vault exec developer \
            -- ${*:1}
}

