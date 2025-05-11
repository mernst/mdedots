all: .hunspell_en_US.sorted .enscriptrc .javadoc-index-files .jsr308-demos.properties .mvc-checkouts \
	.gitconfig-extra-amazon .gitconfig-extra-uwcse \
	.Xresources-cse .Xresources-laptop .Xresources-mac .Xresources-yogaubuntu

showvars:
	@echo "SH_SCRIPTS=${SH_SCRIPTS}"
	@echo "BASH_SCRIPTS=${BASH_SCRIPTS}"

# If the .sorted file is up to date, then the main file is sorted
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
