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
    export PS1="\${debian_chroot:+(\$debian_chroot)}\[\033[33;40m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\] \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\$ "
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
    export PYTHONPATH=.:./src
else
    export PYTHONPATH=.:./src:${PYTHONPATH}
fi

# Tab completion for stack
eval "$(stack --bash-completion-script stack)"

## Configuration for pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYTHONPATH=./src:./test
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
