export ZSH="/home/bart/.oh-my-zsh"
ZSH_THEME="custom"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"
# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(git dotenv zsh-aws-vault docker pyenv kubectl helm z)

source $ZSH/oh-my-zsh.sh

set -o vi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:/opt/protoc/bin:$PATH"
export EDITOR=vim
eval "$(pyenv virtualenv-init -)"

export PATH="$HOME/.pyenv.overrides:$PATH"

source "$HOME/.pyenv/versions/3.6.8/bin/aws_zsh_completer.sh"
export AWS_CONFIG_FILE=/home/bart/.aws.vault/config
export AWS_VAULT_BACKEND=secret-service
export AWS_REGION=eu-west-1
export AWS_DEFAULT_REGION=eu-west-1

if [ -n "$DESKTOP_SESSION" ];then
    eval "$(gnome-keyring-daemon --start --components=secrets)"
    export SSH_AUTH_SOCK
fi

bindkey -v '^?' backward-delete-char
bindkey -v '^R' history-incremental-search-backward
