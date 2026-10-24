# Rules about Git, GitHub, and pull requests

## Git

* Never rewrite git history.  For example, do not rebase.
* When working on a downstream fork of an upstream repository (one that periodically merges from upstream and carries its own changes), strive to minimize the differences from the upstream repository.  That is more important than optimizing the downstream fork.  Examples:  `codespecs/fjalar` (its `valgrind/` subdirectory is downstream of `valgrind`), `typetools/jdk` and `typetools/jdk21u` (of `openjdk/jdk*`), `typetools/stubparser` (of `javaparser/javaparser`), `typetools/guava`, `typetools/asm`, and `codespecs/commons-bcel` (of `apache/commons-bcel`).  `eisop/checker-framework` is downstream of `typetools/checker-framework`, but it intentionally makes design changes, so minimize only incidental differences.  A personal fork that holds only pull-request branches (such as `mernst/daikon`) is not a downstream fork.
* When creating a new branch, create a new directory for it using `git-checkout-branch`.

## Branches

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

## GitHub

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

## Merging

* When resolving a merge conflict, change only the conflicted hunks.  Keep every hunk that git merged cleanly, even in a file that has conflicts.  Do not resolve a file by taking one side wholesale (`git checkout --ours`/`--theirs`, `-X ours`/`-X theirs`, or rewriting the file from one parent).
* Before committing a merge, run `git diff AUTO_MERGE`.  It shows how the working tree differs from git's automatic merge, which still has its conflict markers.  Every change it shows must be inside a conflict region.  Investigate any other change; it usually means that the resolution undid one side's work.
* To audit a merge that is already committed, recreate git's automatic merge with `git merge-tree --write-tree M^1 M^2` and diff the resulting tree against `M`.
