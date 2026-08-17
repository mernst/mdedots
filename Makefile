.PHONY: all clean test default
.PHONY: diff-duplicated-configs diff-dots-with-plume-scripts diff-dots-with-root

# Do nothing by default
default:

# Configuration files that dots/ duplicates from the plume-scripts repository.
PLUME_SCRIPTS_CONFIGS = \
	.coderabbit.yaml \
	.markdownlint-cli2.yaml \
	.pmd-ruleset.xml \
	.pymarkdown \
	.ruff.toml \
	.yamlfmt.yaml \
	.yamllint.yaml \
	prek.toml

# Configuration files that this repository's root duplicates from dots/.
ROOT_CONFIGS = \
	.coderabbit.yaml \
	.ruff.toml \
	.yamlfmt.yaml \
	.yamllint.yaml \
	prek.toml

# Report every difference among the duplicated configuration files.
diff-duplicated-configs:
	@status=0; \
	$(MAKE) --no-print-directory diff-dots-with-plume-scripts || status=1; \
	$(MAKE) --no-print-directory diff-dots-with-root || status=1; \
	exit $$status

# Each `diff` runs even if an earlier one reported a difference, so that one
# invocation lists every file that needs attention.
diff-dots-with-plume-scripts:
	git -C .plume-scripts pull --ff-only
	@status=0; \
	for file in $(PLUME_SCRIPTS_CONFIGS); do \
	  diff -u ".plume-scripts/$$file" "dots/$$file" || status=1; \
	done; \
	exit $$status

diff-dots-with-root:
	@status=0; \
	for file in $(ROOT_CONFIGS); do \
	  diff -u "./$$file" "dots/$$file" || status=1; \
	done; \
	exit $$status
