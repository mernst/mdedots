default: compile-without-mew

EMACS ?= emacs

# EL_FILES does not include $(wildcard mew/*.el); use its own Makefile
EL_FILES = $(wildcard *.el)
EL_FILES := $(filter-out vm-w3m.el, $(EL_FILES))
EL_FILES := $(filter-out mew-mde.el, $(EL_FILES))
EL_FILES := $(filter-out read-spam-email.el, $(EL_FILES))
ELC_FILES = $(patsubst %.el,%.elc,$(EL_FILES))

.PHONY: compile compile-without-mew
compile-without-mew: ${ELC_FILES}
compile: compile-without-mew
	make -C mew

%.elc: %.el
#	emacs -q --batch -l ${HOME}/.emacs --eval "(add-to-list 'load-path \".\")" -f batch-byte-compile $<
	emacs --batch -f batch-byte-compile $<

.PHONY: javadoc-index
javadoc-index:
	${INV}/scripts/javadoc-index-to-alist > $HOME/.javadoc-index.el

clean:
	rm -f *.elc

# It's probably worth doing this periodically, to clean up the code.
# Don't forget to do "make clean" afterward.
bytecompile:
	${MAKE} clean
	${EMACS} -batch -f batch-byte-compile *.el
	${MAKE} clean

TAGS: tags
tags:
	etags *.el

.PHONY: javadoc-index
