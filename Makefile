.PHONY: all clean test default

# Do nothing by default
default:

diff-dots-with-plume-scripts:
	git -C .plume-scripts pull --ff-only
	diff .plume-scripts/.coderabbit.yaml dots/.coderabbit.yaml
	diff .plume-scripts/.markdownlint-cli2.yaml dots/.markdownlint-cli2.yaml
	diff .plume-scripts/.pmd-ruleset.xml dots/.pmd-ruleset.xml
	diff .plume-scripts/.pymarkdown dots/.pymarkdown
	diff .plume-scripts/.ruff.toml dots/.ruff.toml
	diff .plume-scripts/.yamllint.yaml dots/.yamllint.yaml
# TODO: prek.toml
