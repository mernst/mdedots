#!/bin/bash

# ~/.bash_profile is for login shells, like logging in via ssh.
# ~/.bashrc is for non-login shells, such as starting a new xterm after being logged in, or an Emacs shell.

# The default umask is set in /etc/login.defs .
# umask 022
# "umask 002" means files you make look like "rw-rw-r-".
# umask 002


export SHELL=${SHELL:-${BASH:-/bin/bash}}


# Include .profile if it exists.  Sets environment variables and paths.
if [ -f $HOME/.profile ]; then
  source $HOME/.profile
fi

# Include .bashrc if it exists.  Config for interactive command line.
if [ -f $HOME/.bashrc ]; then
  source $HOME/.bashrc
fi

# For Rust
if [ -f "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi
