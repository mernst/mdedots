---
name: split
description: Evaluate whether the current pull request can be divided into smaller pull requests.
---

# Try to split the PR

Can this PR be split into multiple smaller pull requests?
Don't do the split, just report to me.

If and when you split:

* Give credit to original authors, via a "Co-authored-by:" line in the commit message.
* Don't include in the PR description text like "Split out of #8382; written by @mernst."  Just add, at the top of all relevant PR descriptions, "Land before #N." or "Land after #N.".
