#!/bin/bash
# .profile should be compatible with any /bin/sh, incuding bash, dash, and ksh.

# ~/.profile: executed by the command interpreter for login shells.
# .profile has configuration NOT specifically related to bash,
# such as environment variables and PATH.
# Includes anything that should be available to graphical applications.
# It is one-time setup.  .bashrc has per-shell-instance setup.
# Do not source .bashrc from .profile (at least not unconditionally).

# Paths are set in this file.
# Classpath is no longer set (used to be set in .environment).

###########################################################################
### Debugging
###

## I'm not sure what the difference between these two is.
## These are set in both .bashrc and .profile.
# export DEBUGBASH=true
# export DEBUGLOGIN=true

if [ -n "$DEBUGBASH" ]; then echo "entering .profile"; fi

###########################################################################
### Variables
###

# Give group write permission.  Same as: umask 002
#  * Can cause problems.  For example, private files such as .ssh directory
#    must not be group-writable.
#  * Can prevent problems.  For example, CVS repositories should be
#    group-writable.
# umask u=rwx,g=rwx,o=rx

# Don't make useless coredump files.  If you want a coredump,
# say "ulimit -c unlimited" and then cause a segmentation fault.
ulimit -c 0

# Aliases are not expanded when the shell is not interactive, unless the
# expand_aliases shell option is set.  This is quite handy for processes
# run from Emacs; does it cause trouble anywhere else?
shopt -s expand_aliases

# # By default bash takes the error status of the last item in a pipeline.
# # For example, false | true will be considered to have succeeded.
# set -o pipefail

if [ -f /etc/redhat-release ]; then
  if grep -q "CentOS Linux release 7" /etc/redhat-release; then
    CENTOS_VERSION=7
    export CENTOS_VERSION
  fi
fi

###########################################################################
### Paths
###

# It is best practice not to set CLASSPATH globally.

export INSTALLDIR=${HOME}/bin/install
# This is not sufficient because there is also the ~/bin/$(uname)-$(uname -m)
# directory that is shared between different Fedora versions.
# if [ -f /etc/redhat-release ]; then export INSTALLDIR=${INSTALLDIR}-f$(cat /etc/redhat-release | cut -d " " -f 3); fi

# CSAIL
export PATH=/afs/csail/group/pag/software/bin:/afs/csail.mit.edu/@sys/local/bin:$PATH
export PATH=/usr/vmware/bin:$PATH

# Apple Macintosh
export PATH=/usr/texbin:$PATH
# For MacPorts
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

## UW CSE
export PATH=${PATH}:/usr/local/bin:/usr/afsws/bin:/usr/bin/X11
export MANPATH=${MANPATH}:/usr/afsws/man
# $(uname -m)-$(uname) has capitalized "Linux"
export PATH=${HOME}/bin/install/texlive/2023/bin/x86_64-linux:${PATH}

# On Mac, homebrew's Emacs should come before the built-in Emacs, which is old
if [ "$(uname)" = "Darwin" ]; then
  export PATH=/usr/local/bin:${PATH}
fi

if [ "$CENTOS_VERSION" = "7" ]; then
  export PATH=${HOME}/bin/install/ActivePerl-5.28/bin:${PATH}
fi

# These export commands are necessary to avoid Perl warnings; daikon-dev.bashrc
# uses perl.  Only set the locale if it is available.  (If it isn't available,
# how do I avoid the perl warnings?)
if command -v locale >/dev/null 2>&1; then
  if locale -a | grep -q en_US.utf8 >/dev/null; then
    # export LANGUAGE=en_US.UTF-8
    # export LC_ALL=en_US.UTF-8
    export LANGUAGE=en_US.utf8
    export LC_ALL=en_US.utf8
  fi
fi

# Daikon
if [ -f "$HOME/research/invariants/daikon/scripts/daikon-dev.bashrc" ]; then
  if [ -n "$DEBUGBASH" ]; then echo "about to source daikon-dev.bashrc: $HOME/research/invariants/daikon/scripts/daikon-dev.bashrc"; fi
  # I don't know why, but
  #   -d "/afs/csail/group/pag/software"
  # returns true and
  #   -d "/afs/csail/group/pag/software/pkg"
  # returns false, which leads to an error message in daikon.bashrc.
  # export JAVA_HOME=$HOME/java/jdk
  # source $HOME/research/invariants/daikon/scripts/daikon-dev.bashrc
  # export BIBINPUTS=.:$HOME/bib:..:
  if [ -n "$DEBUGBASH" ]; then echo "sourced daikon-dev.bashrc"; fi
fi

# For Maven (mvn)
export PATH=${INSTALLDIR}/apache-maven/bin:${PATH}

# For Snap
export PATH=/snap/bin:${PATH}

# Java
# Make PATH consistent with JAVA_HOME
if [ -n "${JAVA_HOME}" ]; then
  export PATH=${JAVA_HOME}/bin:${PATH}
fi

# Environment variables are inherited by child shells (as are the
# environment variables like PATH and CLASSPATH that are set based on
# them), so features coordinated by environment variables are hard to turn
# off without logging out and back in.

export PATH=$HOME/research/types/annotation-tools/annotation-file-utilities/scripts:$PATH

# Javarifier
export PATH=${PATH}:${HOME}/research/types/javarifier/scripts

# Jtreg test harness
export PATH=$HOME/java/jtreg/linux/bin:$PATH
export PATH=$HOME/bin/install/jtreg/bin:$PATH

export PATH=$HOME/java/jdk/bin:$PATH

export PATH=${INSTALLDIR}/hevea-install/bin:$PATH

export PATH=${INSTALLDIR}/epd-7.3-1-rh5-x86/bin:$PATH
export PATH=${INSTALLDIR}/epd-7.3-1-rh5-x86_64/bin:$PATH

export PATH=$PATH:/usr/local/go/bin

export PATH=$HOME/class/331/CurrentQtr/staff/bin:$HOME/class/331/CurrentQtr/courseware:$HOME/class/331/CurrentQtr/courseware/common:${PATH}
export PATH="${PATH}:$sdi/staff/bin:$sdi/courseware:$sdi/courseware/common"

# Mew (Emacs mail reader)
export PATH=$HOME/emacs/mew/bin:$PATH

export PATH=${PATH}:$HOME/.gem/ruby/gems/rake-10.3.2/bin

export PATH=${PATH}:$HOME/bin/install/go/bin

export PATH=${PATH}:$HOME/bin/install/cov-analysis-linux64-7.7.0/bin

export PATH=${PATH}:$HOME/bin/install/global-6.6.3-install/bin

export PATH="$PATH:$HOME/.local/pipx/venvs/black/bin"
export PATH="$PATH:$HOME/.local/pipx/venvs/html5validator/bin"
export PATH="$HOME/.local/bin:$PATH"
export PATH=${HOME}/.venv/bin:${PATH}
export PATH="$PATH:/root/.local/bin"
if command -v register-python-argcomplete >/dev/null; then
  eval "$(register-python-argcomplete pipx)"
fi

# This is not needed.  Instead, edit end of .bashrc file.
# export PATH="$PATH:$HOME/miniconda3/bin"

## System-specific path customizations

if [ "$(uname)" = "Darwin" ]; then
  export PATH=$HOME/tmp/emacs-22.3/src:$HOME/tmp/emacs-22.3/lib-src:$PATH
  export PATH=/macports/bin:/mpisw/bin:$PATH
fi

if [ "$(/bin/hostname)" = "swspm1502" ]; then
  export PATH=${PATH}:/usr/X11R6/bin
fi

if [ -d /opt/libreoffice7.4/program/ ]; then
  export PATH=$PATH:/opt/libreoffice7.4/program/
fi

export PATH=$HOME/research/testing/defects4j/framework/bin:$PATH:/scratch/mernst/clones/testing/defects4j/framework/bin

export PATH=$PATH:$HOME/bin/install/z3/bin

export PATH=$PATH:/root/.local/bin:$HOME/bin/install/Python-2.7.9-install/bin

export PATH=$HOME/bin/install/git-2.48.1-install:$PATH

# Add .NET Core SDK tools
export PATH="$PATH:$HOME/.dotnet/tools"

export PATH="$PATH:$HOME/bin/install/jdt-language-server-1.41.0/bin"

export PATH="$PATH:$HOME/bin/src/git-tools"

## Use system installation, not my personal one.
# # PDFjam
# export PATH=$HOME/bin/install/pdfjam/bin:$PATH
# export MANPATH=$HOME/bin/install/pdfjam/bin:$MANPATH

# @sys is used on AFS.
if [ -d "$HOME"/bin/@sys ]; then
  export PATH=$HOME/bin/@sys:$PATH
else
  BINUNAMEM=$HOME/bin/$(uname)-$(uname -m)
  export PATH=${BINUNAMEM}:$PATH
fi
if [ -n "$CENTOS_VERSION" ]; then
  BINCENTOS=$HOME/bin/$(uname)-$(uname -m)-CentOS${CENTOS_VERSION}
  export PATH=${BINCENTOS}:$PATH
fi
if [ "$DEBUGLOGIN" ]; then echo "system-specific path: $PATH"; fi
export PATH=$HOME/bin/share:$HOME/bin/src/run-google-java-format:$HOME/bin/src/checklink:$HOME/bin/src/html-tools:$HOME/bin/src/git-scripts:$HOME/bin/src/manage-git-branches:$HOME/bin/src/plume-scripts:$HOME/java/plume-lib/merging/src/main/sh:${PATH}:.

# export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${INSTALLDIR}/gc-7.1-install/lib
# export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${INSTALLDIR}/old-lib
# # Isn't this needed for F15?
# # export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/lib64
export LD_LIBRARY_PATH=${HOME}/.local/lib/:${LD_LIBRARY_PATH}

###########################################################################
### Clean up the path
###

if [ -f "$HOME/bin/src/plume-scripts/path-remove" ]; then
  if [ "$DEBUGLOGIN" ]; then
    echo "about to call path-remove: $PATH"
    command -v perl
  fi
  TRIMMED_PATH=$(echo "$PATH" | "$HOME/bin/src/plume-scripts/path-remove")
  if [ "$DEBUGLOGIN" ]; then echo "called path-remove: \"$TRIMMED_PATH\""; fi
  if [ -n "$TRIMMED_PATH" ]; then
    PATH=$TRIMMED_PATH
    export PATH
  fi
  LD_LIBRARY_PATH=$(echo "$LD_LIBRARY_PATH" | "$HOME/bin/src/plume-scripts/path-remove")
fi

if [ "$DEBUGLOGIN" ]; then echo "path = $PATH"; fi
if [ "$DEBUGLOGIN" ]; then command -v javac; fi

# LD_LIBRARY_PATH is set in ~/.environment .

###########################################################################
### Environment and functions
###

### Shell-independent customizations

if [ "$DEBUGLOGIN" ]; then echo "Sourcing .environment"; fi
if ! [ "$dot_environment_file_read" ]; then # avoid sourcing .environment twice
  . "$(dirname "${BASH_SOURCE[0]}")/.environment"
fi
if [ "$DEBUGLOGIN" ]; then echo "Sourced .environment"; fi

# Site-specific environment varibales (such as PRINTER) are in .bashrc.

###########################################################################
### Processes
###

if ! (curl -s http://127.0.0.1:8384/rest/system/ping | grep '{"ping":"pong"}' >/dev/null 2>&1); then
  # syncthing is not running
  nohup syncthing >"$HOME"/tmp/syncthing.log 2>&1 &
fi

# For Rust
if [ -f "$HOME/.cargo/env" ]; then
  # shellcheck disable=SC1091  # file does not exist on some file systems
  . "$HOME/.cargo/env"
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/mernst/.sdkman"
# shellcheck disable=SC1091  # file does not exist on some file systems
[ -s "/home/mernst/.sdkman/bin/sdkman-init.sh" ] && . "/home/mernst/.sdkman/bin/sdkman-init.sh"
