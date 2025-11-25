all: style-fix style-check git-hooks
	${MAKE} style-check
	${MAKE} -C dots
	${MAKE} -C share
	# Emacs comes last to prevent masking other problems
	${MAKE} -C emacs

style-fix: python-style-fix shell-style-fix
style-check: python-style-check python-typecheck shell-style-check

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: share/mdedots.pre-commit
	cp -pf $< $@
.git/hooks/post-merge: share/mdedots.post-merge
	cp -pf $< $@

PYTHON_FILES:=$(wildcard **/*.py) $(shell grep -r -l --exclude='*.py' --exclude='#*' --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir=.git '^\#! \?\(/bin/\|/usr/bin/env \)python')
PYTHON_FILES_TO_CHECK:=$(filter-out ${lcb_runner},${PYTHON_FILES})
python-style-fix:
ifneq (${PYTHON_FILES},)
	@ruff format ${PYTHON_FILES_TO_CHECK}
	@ruff -q check ${PYTHON_FILES_TO_CHECK} --fix
endif
python-style-check:
ifneq (${PYTHON_FILES},)
	@ruff -q format --check ${PYTHON_FILES_TO_CHECK}
	@ruff -q check ${PYTHON_FILES_TO_CHECK}
endif
python-typecheck:
ifneq (${PYTHON_FILES},)
	@mypy --strict --scripts-are-modules --install-types --non-interactive ${PYTHON_FILES_TO_CHECK} > /dev/null 2>&1 || true
	mypy --strict --scripts-are-modules --ignore-missing-imports ${PYTHON_FILES_TO_CHECK}
endif

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
	@echo "PYTHON_FILES=${PYTHON_FILES}"
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"
