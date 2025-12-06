.PHONY: all

default: all
all: style-fix style-check git-hooks
	${MAKE} -C dots
# Emacs comes last to prevent masking other problems
	${MAKE} -C emacs

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: share/mdedots.pre-commit
	cp -pf $< $@
.git/hooks/post-merge: share/mdedots.post-merge
	cp -pf $< $@

# Code style
SH_SCRIPTS_USER := dots/.aliases dots/.environment dots/.profile
BASH_SCRIPTS_USER := dots/.bashrc dots/.bash_profile
CODE_STYLE_EXCLUSIONS_USER := --exclude-dir apheleia --exclude-dir 'apheleia-*' --exclude-dir=mew --exclude=csail-athena-tickets.bash --exclude=conda-initialize.sh --exclude=addrfilter
ifeq (,$(wildcard .plume-scripts))
dummy != git clone -q https://github.com/plume-lib/plume-scripts.git .plume-scripts
endif
include .plume-scripts/code-style.mak
