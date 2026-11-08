---
name: resolve-merge-conflict
description: Resolve the conflicts of a git merge that is in progress, combining the changes of both sides, or audit a merge that is already committed.  Use whenever a merge, pull, cherry-pick, revert, rebase, or stash application stops with conflicts.
---

# Resolve a merge conflict

Read the file `$HOME/bin/src/manage-git-branches/merge-conflict-instructions.md`, and follow it.
It says how to learn what each side of the merge changed, how to combine the changes, and how to check that no change was lost.
If that file does not exist, read it from <https://github.com/plume-lib/manage-git-branches/blob/main/merge-conflict-instructions.md> instead.
If you can read neither, tell me.
If your instructions already contain that file's contents, as when `git-push-to` asks you to resolve conflicts, follow them, and do not read the file.

## Other operations that stop with conflicts

The instructions are written for `git merge`, in which MERGE_HEAD is the commit being merged in.
Other operations apply the changes of one commit, C, to HEAD.
For them, follow the same instructions, but:

* Wherever the instructions use `git diff HEAD...MERGE_HEAD` (the changes being applied), use `git diff BASE C` instead, where C and BASE are:
  * `git cherry-pick`:  C is CHERRY_PICK_HEAD, and BASE is `CHERRY_PICK_HEAD^`.
  * `git rebase`:  C is REBASE_HEAD, and BASE is `REBASE_HEAD^`.
  * `git revert`:  C is `REVERT_HEAD^`, and BASE is REVERT_HEAD; the changes being applied undo REVERT_HEAD's changes.
  * `git stash apply` or `git stash pop`:  C is the stash being applied, such as `stash@{0}`, and BASE is `stash@{0}^1`, the commit on which the stash was made.
* Wherever the instructions use `git diff MERGE_HEAD...HEAD` (the changes on HEAD), use `git diff BASE HEAD` instead.
  It can be large, because it shows every difference between BASE and HEAD, not just the changes that conflict.
* In Step 3, check only that `git diff HEAD` contains each hunk of `git diff BASE C`, possibly adapted to the changes on HEAD.

## Audit a committed merge

To audit a merge M that is already committed:

* Recreate git's automatic merge with `git merge-tree --write-tree M^1 M^2`, and diff the resulting tree against `M`.
* Do Step 3 of the instructions with `M^1` in place of HEAD, `M^2` in place of MERGE_HEAD, and `M` in place of the working tree:  `git diff M^1 M` should contain each hunk of `git diff M^1...M^2`, and `git diff M^2 M` should contain each hunk of `git diff M^2...M^1`.
