# Code reviews and pull requests

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
