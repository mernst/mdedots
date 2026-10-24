# Rules for editing English text

* Never replace concrete nouns by pronouns.  (But you do not have to replace every pronoun by a noun.)
* Do not expand contractions, but do not introduce them either.
* Retain the fences (three backticks) around fenced code blocks.
* Use technical writing conventions; for example, punctuation is written inside quotation marks if it belongs to the quote, and punctuation is written outside quotation marks if it belongs to the larger sentence.
* When editing, fix errors and clumsiness, but do not trade emphasis or precision for brevity.

## LaTeX

* No `~` is required before `\ref`.

## Markdown

* Every Markdown file that you write should satisfy `markdownlint-cli2`.  Use the configuration in `~/dots/.markdownlint-cli2.yaml` if the directory/project containing the Markdown files does not have its own configuration.  When supplying file names explicitly to `markdownlint-cli2`, use a variant of the configuration that omits the `gitignore: true` setting.

## Conventions for numbers

* It is permissible to use digits, such as “there were 3 blind mice”.  Do not change from digits to words.
* Do not add or remove commas from a number with 4+ digits; that is, both “2093” and “2093” are acceptable.
