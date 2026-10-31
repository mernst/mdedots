---
name: crreview-resolve
description: Address CodeRabbitAI review comments on the current pull request.
---

# Address CodeRabbitAI review comments

<!-- The script share/crreview-resolve implements the same rule for which
comments to act on; keep the two consistent. -->

Examine the CodeRabbitAI reviews for this pull request.

For each CodeRabbitAI review with an unresolved review thread (and also for
the last CodeRabbitAI review, if it has no review threads), address its
"Outside diff range comments" if appropriate.  Ignore CodeRabbitAI reviews
with empty bodies, which hold only replies in review threads.  For each unresolved review
thread that CodeRabbitAI started, consider its comment, address it if
appropriate, and mark it resolved.  There is no need to reply; just resolve
the thread.  Do not act on, reply to, or resolve a review thread that anyone
else started.

Finally, commit and push.
