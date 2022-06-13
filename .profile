source_list=(
  "$HOME/local/rc/profile"
  "$HOME/.local/config/profile"
)

path_list=(
  "$HOME/local/bin"
  "$HOME/.local/bin"
  "$HOME/bin"
  "$HOME/.screenlayout"
  "/opt/bin"
)

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

for path in "${source_list[@]}"
do
  if [ -f "${path}" ]; then
    source "${path}"
  fi
done

for path in "${path_list[@]}"
do
  if [ -d "${path}" ]; then
    PATH="${path}:$PATH"
  fi
done

if [ -f "/home/bart/.ghcup/env" ]; then
  source "/home/bart/.ghcup/env" # ghcup-env
fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:/opt/protoc/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv virtualenv-init -)"
export PATH="$HOME/.pyenv.overrides:$HOME/.pyenv/shims:$PATH"

/usr/bin/xmodmap ~/.Xmodmap

if [ -e /home/bart/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bart/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
