all: git-hooks
	${MAKE} -C dots
	${MAKE} -C share
	# Emacs comes last to prevent masking other problems
	${MAKE} -C emacs

.PHONY: git-hooks
git-hooks: .git/hooks/pre-commit .git/hooks/post-merge
.git/hooks/pre-commit: share/mdedots.pre-commit
	cp -pf $< $@
.git/hooks/post-merge: share/mdedots.post-merge
	cp -pf $< $@

PYTHON_FILES   = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/\|/usr/bin/env \)python'   * | grep -v /.git/ | grep -v '~$$' | grep -v '\.tar$$' | grep -v addrfilter | grep -v cronic-orig | grep -v gradlew | grep -v mail-stackoverflow.sh | grep -v '/old/' | grep -v 'emacs/')
python-style:
	ruff format ${PYTHON_FILES}
	ruff check ${PYTHON_FILES} --fix
