# LLM agent personalization

## Files on disk

* Before responding to a user query, re-read any files that you previously read but have changed since then.
* When asked to write a file or create a document, write it in the current directory.  Do not write it under `/tmp/`, and do not create a cloud document, unless explicitly instructed.

## Shell commands

* Avoid writing or running commands that require my approval; write an equivalent that does not.  For example:
  * Instead of `rm`, write to a fresh scratch directory or overwrite the file.
  * Instead of `cd dir && cmd`, use absolute paths or the tool's own directory option (`make -C`, `git -C`).

## English text

When editing English text (files with extension .txt, .md, .tex, etc.), follow the rules in file `instructions/english-text.md`.
Tell the user if you cannot find that file.

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

When working with Git or GitHub, follow the rules in file `instructions/git.md`.
Tell the user if you cannot find that file.

## Pull requests and code reviews

When working with pull requests or code reviews, follow the rules in file `instructions/code-review.md`.
Tell the user if you cannot find that file.

## Claude Code

The syntax rules for Claude Code `settings.json` files are explained in file ~/wisdom/claude-settings-json.md or at <https://github.com/mernst/uwisdom/blob/master/claude-settings-json.md>.

## Browsers

When the user asks to browse to a URL, don't put in it a LLM-specific tab group.  Just use `xdg-open`.

## Handwriting recognition

There are notes about reading my handwriting in file `instructions/ocr-handwriting-notes.md`.
Tell the user if you cannot find that file.
