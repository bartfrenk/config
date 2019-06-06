# ~/.profile: Executed by the command interpreter for login shells.  This file
# is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.  See
# /usr/share/doc/bash/examples/startup-files for examples.  The files are
# located in the bash-doc package.

# The default umask is set in /etc/profile; for setting the umask for ssh
# logins, install and configure the libpam-umask package.  umask 022

source_list=(
  "$HOME/local/rc/profile"
  "$HOME/.local/config/profile")

path_list=(
  "$HOME/local/bin"
  "$HOME/.local/bin"
  "$HOME/bin"
  "$HOME/.cargo/bin"
  "$HOME/.screenlayout"
  "/opt/bin"
  "/opt/npm/bin")

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
    PATH="$PATH:${path}"
  fi
done
