# Claude personalization

## Files on disk

* Before responding to a user query, re-read any files that you previously read but have changed since then.
* When asked to write a file, default to writing it in the current directory, not under `/tmp/`.

## English text

* Never replace concrete nouns by pronouns.  (But you do not have to replace every pronouns by a noun.)
* Do not expand contractions, but do not introduce them either.
* Retain the fences (three backticks) around fenced code blocks.
* Use technical writing conventions; for example, punctuation is written inside quotation marks if it belongs to the quote, and punctuation is written outside quotation marks if it belongs to the larger sentence.

### LaTeX

* No `~` is required before `\ref`.

### Conventions for numbers

* It is permissible to use digits, such as “there were 3 blind mice”.
* A 4-digit number does not require a comma; “2093” is acceptable.
* Do not place commas within numbers in LaTeX tables.

## Programming

### Branches

Do not change the branch of any existing git repository or clone or working copy.
For example, do not run `git switch` or `git checkout [-b] <branch-name>` in any pre-existing directory.
When creating a new branch, use a new directory or a new clone.

### Java warning suppressions

* The justification for a Checker-Framework-related type-checking warning suppression must start on the same line as the `@SuppressWarnings` key string, and should ideally be brief enough to fit on that same line.

### Code reviews

* In code reviews, do not complain about an empty pull request description.
* When addressing a code review comment from GitHub:
  * If the code review comment is valid and you take action on it, do not reply, but merely click "resolved".
  * If the code review comment is flawed in some way and you do not take action on it, then reply.

### Historical comments

* Do not write comments about how the code used to work.  Comments should focus on how it works now, and (occasionally) how it would fail if the implementation were changed.

### Tests

* Use ".goal" as a suffix for goal/gold/expected files that show what a test should produce.  Do not use ".out" as a suffix for such files.

### The Checker Framework

* When running tests on the `checker-framework` repository or its forks (usually found in directories named `$t/checker-framework*`, ignore `slow.typechecking` warnings.
* In the release notes `docs/CHANGELOG.md`:
  * Ordinary bug fixes are not described, nor are ordinary improvements in precison.  Describing every single one would clutter the changelog and obscure the key changes that users should read about.
  * Pull requests should not modify the "### Closed issues" section; it is filled in at release time.
