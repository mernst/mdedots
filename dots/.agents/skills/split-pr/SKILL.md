---
name: split-pr
description: Evaluate whether the current pull request can be divided into smaller pull requests.
---

# Try to split the PR

Can this PR be split into multiple smaller pull requests?
Don't do the split, just report to me.

It is OK to make a split that will lead to merge conflicts.
It is not acceptable to make a split where a merge would silently work (or mostly work) but have undesirable effects.  Do not make a split that requires adding instructions that the user must be careful to follow when doing merges or landing PRs.  Prefer explicit dependences enforced by pulling one PR into another and/or "Land before #N." and "Land after #N." lines.

If I approve of a split:

* For PRs authored by people besides me, add the split suggestion as a comment on the pull request.
* For PRs authored by me:

  * Give credit to original authors, via a "Co-authored-by:" line in the commit message.
  * Add, at the top of all relevant PR descriptions, "Land before #N." or "Land after #N.".
