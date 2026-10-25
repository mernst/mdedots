# Resolve conflicts among dependent PRs

Run @$HOME/bin/share/pr-status.py

See @pr-dependencies.md section "Dependency chains with conflicts".  Where there is a conflict between dependent branches (including with master), merge it and push the result.  Merge in dependency sequence.

Follow the "Merging" rules in `instructions/git.md`.  In particular, run `git diff AUTO_MERGE` before committing each merge, and confirm that every change it shows is inside a conflict region.

Finally, run @$HOME/bin/share/pr-status.py again.
