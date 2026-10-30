#!/usr/bin/env python3
"""Simplify a unified diff, to make it easier for a person to read.

Usage:
  diff-simplify.py [FILE.diff]

Reads FILE.diff, or standard input if FILE.diff is absent, and writes the
simplified diff to standard output.  The output is for reading, not for `patch`
or `git apply`.

The simplifications are:

 * Consolidate interleaved changes.  Within a hunk, two adjacent change groups
   (maximal runs of "-" and "+" lines) can be merged, along with the context
   between them, if the context is shorter than each of the two change groups
   and at least one of the change groups is not uniform (a uniform change group
   has as many "-" lines as "+" lines, as when it edits lines one for one).
   Such merges happen only if they combine at least three change groups into
   one region.  A merged region is written as all of its old lines, then all
   of its new lines.  It also absorbs each following context line that
   consists only of closing brackets and punctuation, such as "}", because
   such a line is often a coincidental match; but it leaves at least one
   context line before the next change group.

 * Show a lengthened line as an addition.  If a run of "-" lines is followed by
   a run of "+" lines, and the first "+" line starts with all of the (non-blank)
   first "-" line, then the "-" line becomes a context line, and the common
   prefix in the "+" line becomes spaces.  So the "+" line shows only the text
   that was appended.  This makes the new side of the hunk show text that is
   not in the new file.

Each hunk header's line counts are updated to match the hunk's lines.
"""

import argparse
import dataclasses
import re
import signal
import sys
from collections.abc import Iterator
from pathlib import Path

HUNK_HEADER = re.compile(r"@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)")

# A merge combines at least this many change groups.  Merging only two change groups often hides
# the context that shows how they relate, such as a method signature between a changed
# documentation comment and a changed method body.
MIN_MERGED_GROUPS = 3

# A line that consists only of closing brackets and punctuation.
CLOSING_LINE = re.compile(r"\s*[)\]}][)\]};,.\s]*")


@dataclasses.dataclass
class Line:
    """One line of a hunk's body."""

    kind: str
    """The kind of line:  " " for context, "-" for an old line, or "+" for a new line."""
    content: str
    """The line, without its kind character and without its line terminator."""
    no_newline: str | None = None
    r"""The "\ No newline at end of file" line that follows this one, if any."""

    def with_kind(self, kind: str) -> "Line":
        """Return a copy of this line, with the given kind.

        Args:
            kind: the kind of the new line

        Returns:
            a copy of this line, with the given kind
        """
        return dataclasses.replace(self, kind=kind)

    def text(self) -> str:
        r"""Return this line as it appears in a diff.

        Returns:
            this line, followed by its "\ No newline at end of file" line if any
        """
        result = self.kind + self.content + "\n"
        if self.no_newline is not None:
            result += self.no_newline + "\n"
        return result


def simplify_diff(text: str) -> str:
    """Simplify a unified diff.

    Args:
        text: a unified diff

    Returns:
        the simplified diff
    """
    # str.splitlines would also split at characters such as form feed.
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    result: list[str] = []
    i = 0
    while i < len(lines):
        match = HUNK_HEADER.fullmatch(lines[i])
        if match is None:
            result.append(lines[i] + "\n")
            i += 1
            continue
        old_count = int(match[2] or "1")
        new_count = int(match[4] or "1")
        body, i = read_hunk_body(lines, i + 1, old_count, new_count)
        body = simplify_hunk(body)
        result.append(hunk_header(match, old_count, new_count, body))
        result.extend(line.text() for line in body)
    return "".join(result)


def read_hunk_body(
    lines: list[str], start: int, old_count: int, new_count: int
) -> tuple[list[Line], int]:
    """Read the body of a hunk.

    Args:
        lines: the lines of the diff
        start: the index of the hunk's first body line
        old_count: the number of old lines in the hunk, according to its header
        new_count: the number of new lines in the hunk, according to its header

    Returns:
        the hunk's body, and the index of the first line after it
    """
    body: list[Line] = []
    i = start
    while i < len(lines):
        line = lines[i]
        if line.startswith("\\") and body:
            body[-1].no_newline = line
        elif (old_count, new_count) != (1, 1) and starts_file_header(lines, i):
            # The hunk's header claims more lines than the hunk has.  (If one old line and one
            # new line remain, the "---" and "+++" lines could be the hunk's last lines.)
            break
        elif (line.startswith(" ") or line == "") and old_count > 0 and new_count > 0:
            # Some tools strip the space from an empty context line.
            body.append(Line(" ", line[1:]))
            old_count -= 1
            new_count -= 1
        elif line.startswith("-") and old_count > 0:
            body.append(Line("-", line[1:]))
            old_count -= 1
        elif line.startswith("+") and new_count > 0:
            body.append(Line("+", line[1:]))
            new_count -= 1
        else:
            break
        i += 1
    return body, i


def starts_file_header(lines: list[str], i: int) -> bool:
    """Return true if lines[i] starts a file's "---" and "+++" lines and its first hunk header.

    Args:
        lines: the lines of the diff
        i: an index into lines

    Returns:
        true if lines[i] starts a file's "---" and "+++" lines and its first hunk header
    """
    return (
        i + 2 < len(lines)
        and lines[i].startswith("--- ")
        and lines[i + 1].startswith("+++ ")
        and HUNK_HEADER.fullmatch(lines[i + 2]) is not None
    )


def hunk_header(
    match: re.Match[str], original_old_count: int, original_new_count: int, body: list[Line]
) -> str:
    """Return the header of a hunk, with line counts that match its body.

    Args:
        match: the match of HUNK_HEADER against the hunk's original header
        original_old_count: the number of old lines, according to the original header
        original_new_count: the number of new lines, according to the original header
        body: the hunk's body

    Returns:
        the hunk's header, followed by a newline
    """
    old_count = sum(1 for line in body if line.kind != "+")
    new_count = sum(1 for line in body if line.kind != "-")
    if old_count == original_old_count and new_count == original_new_count:
        return match[0] + "\n"
    return f"@@ -{match[1]},{old_count} +{match[3]},{new_count} @@{match[5]}\n"


def simplify_hunk(body: list[Line]) -> list[Line]:
    """Simplify the body of a hunk.

    Args:
        body: the body of a hunk

    Returns:
        the simplified body
    """
    return show_lengthened_lines(consolidate(body))


def runs(body: list[Line]) -> Iterator[list[Line]]:
    """Split a hunk's body into alternating context runs and change groups.

    Args:
        body: the body of a hunk

    Yields:
        each maximal run of context lines, and each maximal run of "-" and "+" lines
    """
    run: list[Line] = []
    for line in body:
        if run and (run[-1].kind == " ") != (line.kind == " "):
            yield run
            run = []
        run.append(line)
    if run:
        yield run


def is_uniform(group: list[Line]) -> bool:
    """Return true if a change group has as many "-" lines as "+" lines.

    Args:
        group: a change group

    Returns:
        true if the change group has as many "-" lines as "+" lines
    """
    return sum(1 for line in group if line.kind == "-") * 2 == len(group)


def consolidate(body: list[Line]) -> list[Line]:
    """Merge change groups that are separated by short context.

    Args:
        body: the body of a hunk

    Returns:
        the body, in which each merged region's old lines precede its new lines
    """
    parts = list(runs(body))
    result: list[Line] = []
    i = 0
    while i < len(parts):
        if parts[i][0].kind == " ":
            result.extend(parts[i])
            i += 1
            continue
        # parts[i] is a change group.  parts[j] is the last change group that merges with it.
        j = i
        while (
            j + 2 < len(parts)
            and len(parts[j + 1]) < min(len(parts[j]), len(parts[j + 2]))
            and not (is_uniform(parts[j]) and is_uniform(parts[j + 2]))
        ):
            j += 2
        if j - i < 2 * (MIN_MERGED_GROUPS - 1):
            result.extend(parts[i])
            i += 1
            continue
        region = [line for part in parts[i : j + 1] for line in part]
        # The context lines that follow the region and that it does not absorb.
        rest: list[Line] = []
        if j + 1 < len(parts):
            context = parts[j + 1]
            # Leave at least one context line before the next change group.
            limit = len(context) if j + 2 >= len(parts) else len(context) - 1
            absorbed = 0
            while absorbed < limit and CLOSING_LINE.fullmatch(context[absorbed].content):
                absorbed += 1
            region.extend(context[:absorbed])
            rest = context[absorbed:]
        result.extend(line.with_kind("-") for line in region if line.kind != "+")
        result.extend(line.with_kind("+") for line in region if line.kind != "-")
        result.extend(rest)
        i = j + 2
    return result


def show_lengthened_lines(body: list[Line]) -> list[Line]:
    """Show each line to which text was appended as context plus an addition.

    If a run of "-" lines is followed by a run of "+" lines, and the first "+" line starts with
    all of the first "-" line, which is not blank, then the "-" line becomes a context line and
    the common prefix in the "+" line becomes spaces.  A line's trailing carriage return, as in a
    diff of a file with CRLF line terminators, is not part of the comparison.

    Args:
        body: the body of a hunk

    Returns:
        the body, with each lengthened line shown as context plus an addition
    """
    result = list(body)
    for i, line in enumerate(body):
        if line.kind != "-" or (i > 0 and body[i - 1].kind == "-"):
            continue
        first_new = i
        while first_new < len(body) and body[first_new].kind == "-":
            first_new += 1
        if first_new == len(body) or body[first_new].kind != "+":
            continue
        old = line.content.removesuffix("\r")
        new = body[first_new].content
        if (
            old.strip()
            and len(new.removesuffix("\r")) > len(old)
            and new.startswith(old)
            and line.no_newline is None
        ):
            result[i] = line.with_kind(" ")
            padding = re.sub(r"[^\t]", " ", old)
            result[first_new] = dataclasses.replace(
                body[first_new], content=padding + new[len(old) :]
            )
    return result


def main() -> None:
    """Simplify the diff named on the command line, or the one on standard input."""
    # Exit quietly, rather than with a traceback, when a reader such as `head` or `less` exits.
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    parser = argparse.ArgumentParser(description="Simplify a unified diff, for reading.")
    parser.add_argument("file", nargs="?", help="the diff file (default: standard input)")
    args = parser.parse_args()
    if args.file is None:
        data = sys.stdin.buffer.read()
    else:
        data = Path(args.file).read_bytes()
    # A diff can contain bytes that are not UTF-8; surrogateescape preserves them.
    text = data.decode("utf-8", "surrogateescape")
    sys.stdout.buffer.write(simplify_diff(text).encode("utf-8", "surrogateescape"))


if __name__ == "__main__":
    main()
