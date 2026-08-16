.PHONY: all clean test

GENERATED = .hunspell_en_US.sorted .enscriptrc .javadoc-index-files .jsr308-demos.properties .mvc-checkouts \
	.Xresources-cse .Xresources-laptop .Xresources-mac .Xresources-yogaubuntu

all: $(GENERATED)

# Most generated files are read-only, so "rm -f" (not plain "rm") is required.
clean:
	rm -f $(GENERATED)

# Building every generated file from scratch is the test.
test:
	$(MAKE) clean
	$(MAKE) all

# Sorting prevents merge conflicts.  The .sorted file is only a timestamp
# marker: if it is up to date, then .hunspell_en_US is sorted and duplicate-free.
.hunspell_en_US.sorted: .hunspell_en_US
	LC_ALL=C sort -u -o $< $<
	touch $@

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
