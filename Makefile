all: style-fix style-check git-hooks
	${MAKE} -C dots
	# Emacs comes last to prevent masking other problems
	${MAKE} -C emacs
	${MAKE} style-check

style-fix: python-style-fix shell-style-fix
style-check: python-style-check shell-style-check

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: share/mdedots.pre-commit
	cp -pf $< $@
.git/hooks/post-merge: share/mdedots.post-merge
	cp -pf $< $@

PYTHON_FILES   = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/\|/usr/bin/env \)python'   * | grep -v /.git/ | grep -v '~$$' | grep -v '\.tar$$' | grep -v addrfilter | grep -v cronic-orig | grep -v gradlew | grep -v mail-stackoverflow.sh | grep -v '/old/' | grep -v 'emacs/')
python-style-fix:
	@ruff -q format ${PYTHON_FILES}
	@ruff -q check ${PYTHON_FILES} --fix
python-style-check:
	@ruff -q format --check ${PYTHON_FILES}
	@ruff -q check ${PYTHON_FILES}


SH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)sh' * | grep -v /.git/ | grep -v '~$$' | grep -v addrfilter | grep -v mail-stackoverflow.sh | grep -v mew.texi | grep -v emacs/mew/ | grep -v conda-initialize.sh)
BASH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)bash' * | grep -v /.git/ | grep -v '~$$' | grep -v emacs/mew/)

shell-style-fix:
	@shfmt -w -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
	@shellcheck -x -P SCRIPTDIR --format=diff ${SH_SCRIPTS} ${BASH_SCRIPTS} | patch -p1

shell-style-check:
	@shfmt -d -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
	@shellcheck -x -P SCRIPTDIR --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
	@checkbashisms -l ${SH_SCRIPTS}

showvars:
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"
