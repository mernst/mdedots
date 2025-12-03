default: all
all: style-fix style-check git-hooks
	${MAKE} -C dots
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

PYTHON_FILES:=$(wildcard **/*.py) $(shell grep -r -l --exclude-dir=.git --exclude-dir=.plume-scripts --exclude-dir=.venv --exclude='*.py' --exclude='#*' --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude=lcb_runner '^\#! \?\(/bin/\|/usr/bin/\|/usr/bin/env \)python')
python-style-fix: .plume-scripts
ifneq (${PYTHON_FILES},)
	@.plume-scripts/cronic ruff --version
	@.plume-scripts/cronic ruff format ${PYTHON_FILES}
	@.plume-scripts/cronic ruff check ${PYTHON_FILES} --fix
endif
.ruff.toml: dots/.ruff.toml
	ln -s $< $@
python-style-check: .plume-scripts .ruff.toml
ifneq (${PYTHON_FILES},)
	@.plume-scripts/cronic ruff --version
	@.plume-scripts/cronic ruff format --check ${PYTHON_FILES}
	@.plume-scripts/cronic ruff check ${PYTHON_FILES}
endif
python-typecheck: .plume-scripts
ifneq (${PYTHON_FILES},)
	@.plume-scripts/cronic mypy --version
	@.plume-scripts/cronic mypy --strict --scripts-are-modules --install-types --non-interactive ${PYTHON_FILES} > /dev/null 2>&1 || true
	@.plume-scripts/cronic mypy --strict --scripts-are-modules --ignore-missing-imports ${PYTHON_FILES}
endif

# Explicitly mention some scripts that don't have a shebang line.
SH_SCRIPTS   := $(shell grep -r -l --exclude='#*' --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir apheleia --exclude-dir 'apheleia-*' --exclude-dir=mew --exclude-dir=.git --exclude-dir=.plume-scripts --exclude-dir=old '^\#! \?\(/bin/\|/usr/bin/env \)sh'   | grep -v addrfilter | grep -v conda-initialize.sh | grep -v cronic-orig | grep -v mail-stackoverflow.sh) dots/.aliases dots/.environment dots/.profile 
BASH_SCRIPTS := $(shell grep -r -l --exclude='#*' --exclude='*~' --exclude='*.tar' --exclude=gradlew --exclude-dir apheleia --exclude-dir 'apheleia-*' --exclude-dir=mew --exclude-dir=.git --exclude-dir=.plume-scripts --exclude-dir=old '^\#! \?\(/bin/\|/usr/bin/env \)bash' | grep -v addrfilter | grep -v conda-initialize.sh | grep -v cronic-orig | grep -v mail-stackoverflow.sh) dots/.bashrc dots/.bash_profile
CHECKBASHISMS := $(shell if command -v checkbashisms > /dev/null ; then \
	  echo "checkbashisms" ; \
	else \
	  wget -q -N https://homes.cs.washington.edu/~mernst/software/checkbashisms && \
	  mv checkbashisms .checkbashisms && \
	  chmod +x ./.checkbashisms && \
	  echo "./.checkbashisms" ; \
	fi)

shell-style-fix: .plume-scripts
ifneq ($(SH_SCRIPTS)$(BASH_SCRIPTS),)
	@.plume-scripts/cronic shfmt -w -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
	@.plume-scripts/cronic shellcheck -x -P SCRIPTDIR --format=diff ${SH_SCRIPTS} ${BASH_SCRIPTS} | patch -p1
endif
shell-style-check: .plume-scripts
ifneq ($(SH_SCRIPTS)$(BASH_SCRIPTS),)
	@.plume-scripts/cronic shfmt -d -i 2 -ci -bn -sr ${SH_SCRIPTS} ${BASH_SCRIPTS}
	@.plume-scripts/cronic shellcheck -x -P SCRIPTDIR --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
endif
ifneq ($(SH_SCRIPTS),)
	@.plume-scripts/cronic ${CHECKBASHISMS} -l ${SH_SCRIPTS}
endif

plume-scripts-update: .plume-scripts
	@git -q -C .plume-scripts pull --ff-only

.plume-scripts:
	git clone -q https://github.com/plume-lib/plume-scripts.git .plume-scripts

showvars:
	@echo "PYTHON_FILES=${PYTHON_FILES}"
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"
