#!/bin/bash
# shellcheck disable=SC2154,SC2181 # because of conda, see below

# ~/.bashrc: anything desired for interactive command line:
# command prompt, EDITOR variable, bash aliases.

# Should do output only in interactive mode; test for it using [[ $- == *i* ]].
# Sourced when starting a new xterm after being logged in, or an Emacs shell.
# By contrast, ~/.bash_profile is for login shells, like logging in via ssh.

# Most customizations (e.g., to PATH) appear in ~/.profile .
# Aliases appear in ~/.aliases .

###########################################################################
### Debugging
###

## I'm not sure what the difference between these two is.
## These are set in both .bashrc and .profile.
export DEBUGBASH=
# export DEBUGBASH=true
export DEBUGLOGIN=
# export DEBUGLOGIN=true

###########################################################################
### Defaults
###

if [ -n "$DEBUGBASH" ]; then echo "Starting .bashrc"; fi

# /etc/profile is automatically read for login shells, so no need to do that.

# Source global definitions
# As of 2023-08-10, at CSE this changes the PATH (putting
# /usr/lib/java/apache-maven-3.8.6/bin at its beginning, for example).
if [ -f /etc/bashrc ]; then
  # shellcheck disable=SC1091  # file does not exist on some file systems
  . /etc/bashrc
  unset PROMPT_COMMAND
fi
export INSTALLDIR=${HOME}/bin/install
export PATH=${INSTALLDIR}/apache-maven/bin:${PATH}

###########################################################################
### Noninteractive shells
###

shopt -s expand_aliases

if [ -d /cygdrive/c/ ]; then
  export DISPLAY=localhost:0
elif [ -d /home/mernst/ ]; then
  if grep -qE "(Microsoft|WSL)" /proc/version &> /dev/null; then
    # Windows Subsystem for Linux
    export DISPLAY=:0
  fi
fi

if [ -n "$DEBUGLOGIN" ]; then echo "Sourcing .aliases"; fi
if [ -f "$HOME/.aliases" ]; then
  if ! alias eth0 > /dev/null 2>&1; then
    source "$HOME/.aliases"
  fi
fi
if [ -n "$DEBUGLOGIN" ]; then echo "Sourced .aliases"; fi

if [ -n "$DEBUGLOGIN" ]; then echo "Sourcing .environment"; fi
if ! [ "$dot_environment_file_read" ]; then # avoid sourcing .environment twice
  . "$HOME/.environment"
fi
if [ -n "$DEBUGLOGIN" ]; then echo "Sourced .environment"; fi

html2ps() { command htmldoc -f "$(basename "$@" .ps)" --webpage "$@"; }
html2pdf() { command htmldoc -f "$(basename "$@" .pdf)" --webpage "$@"; }

# added by travis gem
# shellcheck disable=SC1091  # file does not exist on some file systems
[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

export NVM_DIR="$HOME/.nvm"
# shellcheck disable=SC1091  # file does not exist on some file systems
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
# shellcheck disable=SC1091  # file does not exist on some file systems
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

if [ -n "$DEBUGBASH" ]; then
  echo "In .bashrc: PATH = ${PATH}"
  echo "End of .bashrc section \"noninteractive shells\""
fi

###########################################################################
### Interactive shells
###

# If not running interactively, return.
if [ -z "$PS1" ]; then
  return
fi

if [ -n "$DEBUGLOGIN" ]; then echo "Start of .bashrc section \"interactive shells\""; fi

# Set the prompt to show the host name and event number.
export PS1="\h \#% "

# Don't put duplicate lines in the history.
# export HISTCONTROL=ignoredups
# Supercedes the above.
export HISTIGNORE="[   ]*:&:bg:fg"

if [ -d /homes/gws/mernst ]; then
  export PRINTER=pg270
  # export PRINTER=pgc270
  # export PRINTER=ps541
  # export PRINTER=psc541
fi

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Enable programmable completion features.
if [ -f /etc/bash_completion ]; then
  source /etc/bash_completion
fi

# Set ignoreeof if you don't want EOF as the sole input to the shell to
# immediately signal a quit condition.  This only happens at the start
# of a line if the line is empty, and you haven't just deleted a character
# with C-d.  I turn this on in ~/.bash_profile so that only login shells
# have the right to be obnoxious.
set -o ignoreeof

# Report terminated background jobs immediately (asynchronously).
# set -o notify

# Make it so that failed `exec' commands don't flush this shell.
shopt -s execfail

# HISTSIZE=256

if [ -n "$DEBUGLOGIN" ]; then
  echo "End of .bashrc section \"interactive shells\"; \"extensions\" comes next"
fi

###########################################################################
### Extensions
###

# source $HOME/bin/install/gradle-completion/gradle-completion.bash
# source /usr/local/lib/bazel/bin/bazel-complete.bash

# For Rust
if [ -f "$HOME/.cargo/env" ]; then
  # shellcheck disable=SC1091  # file does not exist on some file systems
  . "$HOME/.cargo/env"
fi

# added by travis gem
# shellcheck disable=SC1091  # file does not exist on some file systems
[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

PATH="/homes/gws/mernst/perl5/bin${PATH:+:${PATH}}"
export PATH
PERL5LIB="/homes/gws/mernst/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/homes/gws/mernst/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base /homes/gws/mernst/perl5"
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=/homes/gws/mernst/perl5"
export PERL_MM_OPT

# `conda activate` won't run unless these lines appear in my .bashrc file --
# even if the lines have already been run, say by running
# `~/dots/conda-initialize.sh`.  However, running these lines for every shell
# interferes with ssh-agent.  So, comment them out with a here-document.
# When using conda, run ~/dots/conda-initialize.sh or run the commands manually.
true << ENDCONDA
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/mernst/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
  eval "$__conda_setup"
else
  if [ -f "/home/mernst/miniconda3/etc/profile.d/conda.sh" ]; then
    . "/home/mernst/miniconda3/etc/profile.d/conda.sh"
  else
    export PATH="/home/mernst/miniconda3/bin:$PATH"
  fi
fi
unset __conda_setup
# <<< conda initialize <<<
ENDCONDA

# The next line updates PATH for the Google Cloud SDK.
# shellcheck disable=SC1091
if [ -f '/home/mernst/bin/install/google-cloud-sdk/path.bash.inc' ]; then . '/home/mernst/bin/install/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
# shellcheck disable=SC1091
if [ -f '/home/mernst/bin/install/google-cloud-sdk/completion.bash.inc' ]; then . '/home/mernst/bin/install/google-cloud-sdk/completion.bash.inc'; fi

# Generated for envman. Do not edit.
# shellcheck disable=SC1091 # files might not exist
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

if [ -n "$DEBUGLOGIN" ]; then
  echo "Exiting .bashrc"
fi
