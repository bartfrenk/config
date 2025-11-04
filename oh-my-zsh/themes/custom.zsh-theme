setopt prompt_subst
autoload -Uz vcs_info
autoload -U add-zsh-hook

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '%F{red}*'   # display this when there are unstaged changes
zstyle ':vcs_info:*' stagedstr '%F{yellow}+'  # display this when there are staged changes
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%c%u%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{white}[%F{2}%b%c%u%F{white}]%f '
zstyle ':vcs_info:svn:*' branchformat '%B'
zstyle ':vcs_info:svn:*' actionformats '%F{5}[%F{2}%b%F{1}:%F{3}%i%F{3}|%F{1}%a%c%u%F{5}]%f '
zstyle ':vcs_info:svn:*' formats '%F{5}[%F{2}%b%F{1}:%F{3}%i%c%u%F{5}]%f '
zstyle ':vcs_info:*' enable git cvs svn

theme_precmd () {
  vcs_info
}

add-zsh-hook precmd theme_precmd

dir_segment() {
    print "%{$fg_bold[yellow]%}%~%{$reset_color%}"
}

git_segment() {
    print "%B${vcs_info_msg_0_}%b"
}

venv_segment() {
    if [[ -n "$VIRTUAL_ENV" ]]; then
        print "%B%F{white}[%F{green}$VIRTUAL_ENV:t%F{white}]%f%b";
    fi
}

prompt_segment() {
  print "%B%F{white}»%f%b"
}

PROMPT='$(dir_segment) $(venv_segment) $(git_segment)
$(prompt_segment) '
