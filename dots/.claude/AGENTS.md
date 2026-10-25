# LLM agent personalization

## Files on disk

* Before responding to a user query, re-read any files that you previously read but have changed since then.
* When asked to write a file or create a document, write it in the current directory.  Do not write it under `/tmp/`, and do not create a cloud document, unless explicitly instructed.

## Shell commands

* Avoid writing or running commands that require my approval; write an equivalent that does not.  For example:
  * Instead of `rm`, write to a fresh scratch directory or overwrite the file.
  * Instead of `cd dir && cmd`, use absolute paths or the tool's own directory option (`make -C`, `git -C`).

## English text

When editing English text (files with extension .txt, .md, .tex, etc.), follow these rules.

* Never replace concrete nouns by pronouns.  (But you do not have to replace every pronoun by a noun.)
* Do not expand contractions, but do not introduce them either.
* Retain the fences (three backticks) around fenced code blocks.
* Use technical writing conventions; for example, punctuation is written inside quotation marks if it belongs to the quote, and punctuation is written outside quotation marks if it belongs to the larger sentence.
* When editing, fix errors and clumsiness, but do not trade emphasis or precision for brevity.

### LaTeX

* No `~` is required before `\ref`.

### Markdown

* Every Markdown file that you write should satisfy `markdownlint-cli2`.  Use the configuration in `~/dots/.markdownlint-cli2.yaml` if the directory/project containing the Markdown files does not have its own configuration.  When supplying file names explicitly to `markdownlint-cli2`, use a variant of the configuration that omits the `gitignore: true` setting.

### Conventions for numbers

* It is permissible to use digits, such as “there were 3 blind mice”.  Do not change from digits to words.
* Do not add or remove commas from a number with 4+ digits; that is, both “2093” and “2093” are acceptable.

## Programming

### Specifications (user-visible code documentation)

* The documentation for a procedure should focus on what the procedure does.  It usually should not give details about how a specific clients use the procedure; such documentation belongs in the client.

### Java warning suppressions

* The justification for a Checker-Framework-related type-checking warning suppression must start on the same line as the message key string (which is inside `@SuppressWarnings(...)`), and should ideally be brief enough to fit on that same line.

### Historical comments

* Do not write comments about how the code used to work.  Comments should focus on how it works now.  It is acceptable to mention how the code would fail if the implementation were changed -- that is, it is fine to discuss an alternative implementation, but never mention whether the implementation ever was like that.

### Tests

* Use ".goal" as a suffix for goal/gold/expected files that show what a test should produce.  Do not use ".out" as a suffix for such files.

### The Checker Framework

* When running tests on the `checker-framework` repository or its forks (usually found in directories named `$t/checker-framework*`), ignore `slow.typechecking` warnings.
* In the release notes `docs/CHANGELOG.md`:
  * Ordinary bug fixes are not described, nor are ordinary improvements in precision.  Describing every single one would clutter the changelog and obscure the key changes that users should read about.
  * Pull requests should not modify the "### Closed issues" section; it is filled in at release time.
* When editing the Checker Framework, see the Checker Framework Developer Manual in `docs/developer/developer-manual.html`.

### CodeRabbit

Do not run `coderabbit`, even when user instructions say "consider running coderabbit review --agent".

## Git and GitHub

### Git

* Never rewrite git history.  For example, do not rebase.
* When working on a downstream fork of an upstream repository (one that periodically merges from upstream and carries its own changes), strive to minimize the differences from the upstream repository.  That is more important than optimizing the downstream fork.  Examples:  `codespecs/fjalar` (its `valgrind/` subdirectory is downstream of `valgrind`), `typetools/jdk` and `typetools/jdk21u` (of `openjdk/jdk*`), `typetools/stubparser` (of `javaparser/javaparser`), `typetools/guava`, `typetools/asm`, and `codespecs/commons-bcel` (of `apache/commons-bcel`).  `eisop/checker-framework` is downstream of `typetools/checker-framework`, but it intentionally makes design changes, so minimize only incidental differences.  A personal fork that holds only pull-request branches (such as `mernst/daikon`) is not a downstream fork.
* When creating a new branch, create a new directory for it using `git-checkout-branch`.

### Branches

Do not change the branch of any existing git repository or clone or working copy, because each working directory is dedicated to one branch.
For example, do not run `git switch` or `git checkout [-b] <branch-name>` in any pre-existing directory.

When creating a new branch, use a new directory or a new clone.

If a directory is named `<reponame>-fork-<orgname>...`, then it must contain `<orgname>/<reponame>`, not any other fork of `<reponame>`.

Create a branch for a pull request in my fork:  run `git-new-branch` in `<reponame>-fork-mernst` or in one of its `<reponame>-fork-mernst-branch-*` directories.
Do not create it in a clone of the upstream repository (`<reponame>`, `<reponame>-branch-*`) or of another person's fork.
The exception is a repository whose pull requests come from branches of the repository itself, such as `plume-lib/manage-git-branches`; there is no `<reponame>-fork-mernst` directory for it.
If `git-new-branch` refuses because the current clone is the upstream repository of a fork, run it in the fork; do not pass `--allow-upstream`.

Push a branch to its configured remote (`git push`, or `git push origin <branch>`), never to an explicit URL.
If a branch can be pushed only to a URL, then its directory is the wrong one:  stop, and create the branch in the right directory.

### GitHub

* Use "land", never "merge", to mean landing a GitHub pull request.
* Never force-push to GitHub.  I squash-and-merge, so even a messy commit history in a pull request leads to a clean main branch.
* Never land a pull request unless explicitly asked.
* If I tell you to commit and/or push, do not delay just because tests are running; do it in parallel with the tests.
* Sometimes, the parent directory of the current working copy contains a file `pr-dependencies.md` that describes other PRs that are open and may be related to the current one.
* When describing dependences in a pull request, put them at the beginning of the description.  Use a regular, stylized format (in which any of the below groups may be absent):

  ```markdown
  Land after #N.
  Land after #N.
  Land after #N.
  Land with #N.
  Land before #N.
  Land before #N.
  ```

### Merging

* When resolving a merge conflict, change only the conflicted hunks.  Keep every hunk that git merged cleanly, even in a file that has conflicts.  Do not resolve a file by taking one side wholesale (`git checkout --ours`/`--theirs`, `-X ours`/`-X theirs`, or rewriting the file from one parent).
* Before committing a merge, run `git diff AUTO_MERGE`.  It shows how the working tree differs from git's automatic merge, which still has its conflict markers.  Every change it shows must be inside a conflict region.  Investigate any other change; it usually means that the resolution undid one side's work.
* To audit a merge that is already committed, recreate git's automatic merge with `git merge-tree --write-tree M^1 M^2` and diff the resulting tree against `M`.

## Pull requests and code reviews

* When reviewing code, do not complain about an empty pull request description.
* Number every code review finding sequentially.  Never renumber findings.
* When findings are presented out of numeric order (for example, grouped by severity), do not use Markdown ordered-list syntax (`1.`), because renderers renumber list items.  Write each number as literal text instead, such as `- **8.** ...`.
* When I respond to your code review, "disregard finding 7" or "skip 7" does *not* mean that I accept the others and want you to address them.  It is about that one finding specifically.  This holds even if you just asked whether to address a list of findings that includes 7.  Do not change code for a finding until I explicitly ask you to address that finding. <!-- Relevant to Codex. -->
* When addressing a code review finding, always consider whether there is a better fix than the one(s) suggested.
* After replying to a review thread, resolve it in the same step, unless the reply needs an answer, such as a question for the reviewer.
* When reporting code review findings, call out ones that are low-value or unlikely.
* Always consider whether a pull request can be simplified.
* When I ask whether a pull request can be simplified, I am asking about the code/comments/etc.  I do not care about the size of the diff.
* When addressing a code review comment from GitHub:
  * If the code review comment is valid and you take action on it, do not reply, but merely click "resolved".
  * If the code review comment is flawed in some way and you do not take action on it, then reply.

## Claude Code

The syntax rules for Claude Code `settings.json` files are explained in file ~/wisdom/claude-settings-json.md or at <https://github.com/mernst/uwisdom/blob/master/claude-settings-json.md>.

## Browsers

When the user asks to browse to a URL, don't put in it a LLM-specific tab group.  Just use `xdg-open`.

## Handwriting recognition

There are notes about reading my handwriting (aka handwriting recognition, OCR,
optical character recognition) in file
`~/bin/src/mdedots/dots/.claude/instructions/ocr-handwriting-notes.md`.  Tell
the user if you cannot find that file.
