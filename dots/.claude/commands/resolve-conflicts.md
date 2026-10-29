# Resolve conflicts among dependent PRs

Run @$HOME/bin/share/pr-status.py .

See @pr-dependencies.md section "Dependency chains with conflicts".

Run each chain (one `git-push-to` command) independently, in a separate subagent.

Where there is a conflict between dependent branches (including with master), merge it and push the result.  Follow the "Merging" rules in the global `CLAUDE.md`.  In particular, run `git diff AUTO_MERGE` before committing each merge, and confirm that every change it shows is inside a conflict region.  If you need to do work, such as editing or running commands, do it in the existing directories; that is, don't make a new working copy.

Repeat until the chain completes without errors.

Finally, run @$HOME/bin/share/pr-status.py again.  If the "Dependency chains with conflicts" is not empty, start over.
