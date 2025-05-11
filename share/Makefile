SH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)sh' * | grep -v .git | grep -v "~" | grep -v addrfilter | grep -v mail-stackoverflow.sh | grep -v '^old/' | grep -v '^#')
BASH_SCRIPTS = $(shell grep -r -l '^\#! \?\(/bin/\|/usr/bin/env \)bash' * | grep -v .git | grep -v "~" | grep -v '^old/')
shell-script-style:
	shellcheck --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
	checkbashisms ${SH_SCRIPTS}
showvars:
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"
