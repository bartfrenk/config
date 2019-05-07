# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# See /usr/share/doc/bash/examples/startup-files for examples.
# The files are located in the bash-doc package.

# The default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

if [ -d "/opt/bin" ]; then
    PATH="/opt/bin:$PATH"
fi

if [ -f "$HOME/local/rc/profile" ]; then
    source "$HOME/local/rc/profile"
fi

if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/local/bin" ]; then
    PATH="$HOME/local/bin:$PATH"
fi

if [ -d "/opt/adr-tools-3.0.0/src" ]; then
    PATH="/opt/adr-tools-3.0.0/src:$PATH"
fi



if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.screenlayout/" ]; then
    PATH="$HOME/.screenlayout/:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin/:$PATH"
fi
