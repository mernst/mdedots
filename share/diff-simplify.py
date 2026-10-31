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
   and at least one of the change groups is not balanced (a balanced change
   group has as many "-" lines as "+" lines, as when it edits lines one for
   one).  Two balanced change groups are not merged, so a series of one-for-one
   edits, such as a renaming on several lines, stays interleaved with its
   context, where it is easiest to read.  Merges happen only if they combine at
   least three change groups into one region.  A merged region is written as
   all of its old lines, then all of its new lines.  It also absorbs each
   following context line that consists only of closing brackets and
   punctuation, such as "}" or "):", because such a line is often a
   coincidental match; but it leaves at least one context line before the next
   change group.

 * Blank out a common prefix.  If, in a change group that is not
   merged, a run of "-" lines is followed by a run of "+" lines, and the first
   "+" line starts with all of the (non-blank) first "-" line, then the "-"
   line becomes a context line, and the common prefix in the "+" line becomes
   whitespace of the same display width.  So the "+" line shows only the text
   that was appended.  This makes the new side of the hunk show text that is
   not in the new file.

Each hunk header's line counts are updated to match the hunk's lines.
"""

import argparse
import dataclasses
import re
import signal
import sys
import unicodedata
from pathlib import Path

HUNK_HEADER = re.compile(r"@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)")

# The start of the file name in a "---" or "+++" line:  a prefix such as "a/" (git), /dev/null,
# or a name followed by a tab and a timestamp (diff -u).
FILE_HEADER_NAME = re.compile(r"[a-z]/|/dev/null|[^\t]*\t")

# A merge combines at least this many change groups.  Merging only two change groups often hides
# the context that shows how they relate, such as a method signature between a changed
# documentation comment and a changed method body.
MIN_MERGED_GROUPS = 3

# A line that consists only of closing brackets and punctuation, and that is not blank.
CLOSING_LINE = re.compile(r"\s*[)\]};:,.][)\]};:,.\s]*")


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
        body, i = read_hunk_body(lines, i + 1, *header_counts(match))
        body = merge_change_groups(body)
        result.append(hunk_header(match, body))
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
        elif ends_hunk(lines, i, old_count, new_count):
            break
        elif line.startswith(" ") and old_count > 0 and new_count > 0:
            body.append(Line(" ", line[1:]))
            old_count -= 1
            new_count -= 1
        elif line in {"", "\r"} and old_count > 0 and new_count > 0:
            # Some tools strip the space from an empty context line.
            body.append(Line(" ", line))
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


def ends_hunk(lines: list[str], i: int, old_count: int, new_count: int) -> bool:
    """Return true if lines[i] follows a hunk whose header claims more lines than the hunk has.

    Diff tools do not produce such a header, but editing a .diff file can, for example by deleting
    lines from a hunk without updating its header.

    Such a line looks like a hunk line, but it is a file's "---" line, or a separator that is
    followed by a line that cannot continue the hunk:  a blank line, or the "-- " that precedes
    the signature of a patch from `git format-patch`.  A line that does not look like a hunk line,
    such as an "@@" hunk header, needs no check here, because read_hunk_body already stops at it.

    Args:
        lines: the lines of the diff
        i: the index of a line that might be within a hunk
        old_count: the number of old lines that remain in the hunk, according to its header
        new_count: the number of new lines that remain in the hunk, according to its header

    Returns:
        true if lines[i] follows a hunk whose header claims more lines than the hunk has
    """
    line = lines[i]
    if (line in {"", "\r"} and (old_count, new_count) != (1, 1)) or (
        line == "-- " and (old_count, new_count) != (1, 0)
    ):
        return i + 1 == len(lines) or not can_be_in_hunk(lines[i + 1])
    if not (
        i + 2 < len(lines)
        and line.startswith("--- ")
        and lines[i + 1].startswith("+++ ")
        and HUNK_HEADER.fullmatch(lines[i + 2]) is not None
    ):
        return False
    # If one old line and one new line remain, the "---" and "+++" lines could be the hunk's
    # last lines.  They are a file header only if they name files the way that diff tools do.
    return (old_count, new_count) != (1, 1) or all(
        FILE_HEADER_NAME.match(header[4:]) for header in lines[i : i + 2]
    )


def can_be_in_hunk(line: str) -> bool:
    """Return true if a line can be part of a hunk's body.

    Args:
        line: a line of a diff

    Returns:
        true if the line can be part of a hunk's body
    """
    return line[:1] in {" ", "-", "+", "\\"} or line in {"", "\r"}


def header_counts(match: re.Match[str]) -> tuple[int, int]:
    """Return the line counts in a hunk header.

    Args:
        match: the match of HUNK_HEADER against a hunk header

    Returns:
        the number of old lines and the number of new lines, according to the header
    """
    return int(match[2] or "1"), int(match[4] or "1")


def hunk_header(match: re.Match[str], body: list[Line]) -> str:
    """Return the header of a hunk, with line counts that match its body.

    Args:
        match: the match of HUNK_HEADER against the hunk's original header
        body: the hunk's body

    Returns:
        the hunk's header, followed by a newline
    """
    old_count = sum(1 for line in body if line.kind != "+")
    new_count = sum(1 for line in body if line.kind != "-")
    if (old_count, new_count) == header_counts(match):
        return match[0] + "\n"
    return f"@@ -{match[1]},{old_count} +{match[3]},{new_count} @@{match[5]}\n"


def runs(body: list[Line]) -> list[list[Line]]:
    """Split a hunk's body into alternating context runs and change groups.

    Args:
        body: the body of a hunk

    Returns:
        each maximal run of context lines, and each maximal run of "-" and "+" lines, in order
    """
    result: list[list[Line]] = []
    for line in body:
        if result and (result[-1][-1].kind == " ") == (line.kind == " "):
            result[-1].append(line)
        else:
            result.append([line])
    return result


def is_balanced(group: list[Line]) -> bool:
    """Return true if a change group has as many "-" lines as "+" lines.

    Args:
        group: a change group

    Returns:
        true if the change group has as many "-" lines as "+" lines
    """
    return sum(1 for line in group if line.kind == "-") * 2 == len(group)


def merge_change_groups(body: list[Line]) -> list[Line]:
    """Merge a hunk's change groups that are separated by short context.

    Also blanks out common prefixes, using blank_common_prefix, in each change group that is not
    merged.  A merged region's first old line and first new line can come from different change
    groups, so they are not compared.

    Args:
        body: the body of a hunk

    Returns:
        the simplified body, in which each merged region's old lines precede its new lines
    """
    # Alternating context runs and change groups.
    parts = runs(body)
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
            and not (is_balanced(parts[j]) and is_balanced(parts[j + 2]))
        ):
            j += 2
        if j - i < 2 * (MIN_MERGED_GROUPS - 1):
            result.extend(blank_common_prefix(parts[i]))
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


def blank_common_prefix(group: list[Line]) -> list[Line]:
    """Blank out the prefix that a "+" line shares with a "-" line that the "+" line extends.

    If a run of "-" lines is followed by a run of "+" lines, and the first "+" line starts with
    all of the first "-" line, which is not blank, then the "-" line becomes a context line and
    the common prefix in the "+" line becomes whitespace of the same display width.  A line's
    trailing carriage return, as in a diff of a file with CRLF line terminators, is not part of
    the comparison, but the two lines must both have one or both lack one.

    Args:
        group: a change group

    Returns:
        the change group, with each common prefix blanked out
    """
    result = list(group)
    for i, line in enumerate(group):
        if line.kind != "-" or (i > 0 and group[i - 1].kind == "-"):
            continue
        first_new = i
        while first_new < len(group) and group[first_new].kind == "-":
            first_new += 1
        if first_new == len(group) or group[first_new].kind != "+":
            continue
        old = line.content.removesuffix("\r")
        new = group[first_new].content
        if (
            old.strip()
            and len(new.removesuffix("\r")) > len(old)
            and new.startswith(old)
            and new.endswith("\r") == line.content.endswith("\r")
            and line.no_newline is None
        ):
            result[i] = line.with_kind(" ")
            result[first_new] = dataclasses.replace(
                group[first_new], content=whitespace_like(old) + new[len(old) :]
            )
    return result


def whitespace_like(text: str) -> str:
    """Return whitespace that occupies the same columns as some text, when displayed.

    Each tab stays a tab.  Each other character becomes as many spaces as the columns that it
    occupies in a terminal:  two for a wide character such as a CJK ideograph or most emoji, and
    none for a zero-width character such as a combining mark.

    Args:
        text: the text whose columns to fill

    Returns:
        whitespace that occupies the same columns as the text
    """
    result: list[str] = []
    for char in text:
        if char == "\t":
            result.append("\t")
        elif unicodedata.category(char) in {"Mn", "Me", "Cf"}:
            continue
        elif unicodedata.east_asian_width(char) in {"W", "F"}:
            result.append("  ")
        else:
            result.append(" ")
    return "".join(result)


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
