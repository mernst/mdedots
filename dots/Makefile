all: .hunspell_en_US.sorted .enscriptrc .javadoc-index-files .jsr308-demos.properties .mvc-checkouts \
	.gitconfig-extra-amazon .gitconfig-extra-uwcse \
	.Xresources-cse .Xresources-laptop .Xresources-mac .Xresources-yogaubuntu

# Explicitly mention some scripts that don't have a shebang line.
STARTUP_SCRIPTS =
SH_SCRIPTS = .aliases .environment .profile $(shell grep -r -l --exclude-dir emacs/apheleia --exclude-dir emacs/apheleia-* '^\#! \?\(/bin/\|/usr/bin/env \)sh' * | grep -v .git | grep -v "~" | grep -v addrfilter | grep -v mail-stackoverflow.sh)
BASH_SCRIPTS = .bashrc .bash_profile $(shell grep -r -l --exclude-dir emacs/apheleia --exclude-dir '^\#! \?\(/bin/\|/usr/bin/env \)bash' * | grep -v .git | grep -v "~")
shell-script-style:
	shellcheck -x -P ${HOME}/dots --format=gcc ${SH_SCRIPTS} ${BASH_SCRIPTS}
	checkbashisms ${SH_SCRIPTS}
showvars:
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"

# If the .sorted file is up to date, then the main file is sorted.
# Sorting prevents merge conflicts.
.hunspell_en_US.sorted: .hunspell_en_US
	LC_ALL=C sort < $< | uniq > $@ && cp -pf $@ $<

.enscriptrc: .enscriptrc-template
	@rm -rf $@
	cp $< $@
	echo "StatesConfigFile: ${HOME}/dots/enscript.st" >> .enscriptrc
	chmod -w $@

.javadoc-index-files: .javadoc-index-files.env
	@rm -rf $@
	envsubst < $< > $@
	chmod -w $@

.jsr308-demos.properties: .jsr308-demos.properties.env
	@rm -rf $@
	envsubst < $< > $@
	chmod -w $@

.mvc-checkouts: .mvc-checkouts.env
	@rm -rf $@
	envsubst < $< > $@
	chmod -w $@

.Xresources-cse: .Xresources-cpp
	@rm -rf $@
	cpp -P -Dcse $< $@
	chmod -w $@

.Xresources-laptop: .Xresources-cpp
	@rm -rf $@
	cpp -P -Dlaptop $< $@
	chmod -w $@

.Xresources-mac: .Xresources-cpp
	@rm -rf $@
	cpp -P -Dlaptop -Dmac $< $@
	chmod -w $@

.Xresources-yogaubuntu: .Xresources-cpp
	@rm -rf $@
	cpp -P -Dlaptop -Dyogaubuntu $< $@
	chmod -w $@
