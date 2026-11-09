"""Tests for checkout-conflicting-prs that do not run git or access GitHub."""

# The PR status document's table rows are long, as pr-status writes them.
# ruff: file-ignore[line-too-long]

from __future__ import annotations

import sys
import unittest
from pathlib import Path
from unittest import mock

SCRIPT_DIRECTORY = Path(__file__).parent
if str(SCRIPT_DIRECTORY) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIRECTORY))

from script_loader import load_script  # ruff: ignore[module-import-not-at-top-of-file]

ccp = load_script("checkout-conflicting-prs")

Pr = ccp.Pr
Branch = ccp.Branch

CF = "typetools/checker-framework"


def link(number: int, slug: str = CF) -> str:
    """Return a Markdown link to the given PR, as pr-status writes it.

    Returns:
        A link whose text is the PR's reference, such as "cf#3".
    """
    return f"[cf#{number}](https://github.com/{slug}/pull/{number})"


DOCUMENT = f"""\
# PR status

## Unblocked PRs (checker-framework)

| PR | Assigned to | Unresolved threads | Title | Directory | Needed by | Textual conflicts | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| {link(1)} | mernst | ✅ | No conflict | (none) | | none | |
| {link(2)} | | 🔀 | Conflicts with master \\| has a bar | (none) | | `A.java` with `master` | |
| {link(3)} | | ❌ 🔀 | Conflicts, has a working copy | `/x/cf-branch-three/` | | none | |

## Blocked PRs (checker-framework)

| PR | Assigned to | Unresolved threads | Title | Directory | Depends on | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| {link(4)} | | 🔀 | Conflicts with a prerequisite | (none) | {link(3)} ❌ 🔀 and {link(1)} (and transitively {link(5)}) | |
| {link(6)} | | 🔀 | Conflicts with master | (none) | {link(1)} (and `master` 🔀) | |

## Other directories

| Directory | Status | Relationship to other branches |
| --- | --- | --- |
| `cf` | a fork's `master` | none |
"""


class TestParsing(unittest.TestCase):
    """Tests of the parsing of pr-status.md."""

    def test_split_row(self):
        """Escaped bars do not separate cells."""
        self.assertEqual(ccp.split_row("| a | b \\| c |  |"), ["a", "b \\| c", ""])

    def test_conflicting_prerequisites(self):
        """Only links followed by shuffle arrows are conflicting prerequisites."""
        cell = f"{link(3)} ❌ 🔀 and {link(1)} (and transitively {link(5)})"
        self.assertEqual(ccp.conflicting_prerequisites(cell), (Pr(CF, 3),))
        self.assertEqual(ccp.conflicting_prerequisites(f"{link(1)} (and `master` 🔀)"), ())
        self.assertEqual(ccp.conflicting_prerequisites(""), ())

    def test_table_rows(self):
        """Every PR table row is read, and other tables are ignored."""
        rows = {row.pr.number: row for row in ccp.table_rows(DOCUMENT)}
        self.assertEqual(sorted(rows), [1, 2, 3, 4, 6])
        self.assertFalse(rows[1].conflict)
        self.assertTrue(rows[2].conflict)
        self.assertFalse(rows[2].has_working_copy)
        self.assertTrue(rows[3].has_working_copy)
        self.assertEqual(rows[4].conflicting_prerequisites, (Pr(CF, 3),))
        self.assertEqual(rows[6].conflicting_prerequisites, ())


class TestNeededBranches(unittest.TestCase):
    """Tests of the choice of branches to check out."""

    def test_needed_branches(self):
        """A conflicting PR needs its branch and its upstream branch."""
        heads = {n: Branch(CF, "someone", f"branch-{n}") for n in range(1, 7)}
        master = Branch(CF, "typetools", "master")
        copies = mock.Mock()
        copies.has_branch.return_value = False
        with (
            mock.patch.object(ccp, "head_branch", side_effect=lambda pr: heads[pr.number]),
            mock.patch.object(ccp, "base_branch", return_value=master),
        ):
            branches, failures = ccp.needed_branches(ccp.table_rows(DOCUMENT), copies)
        # PR 3 has a working copy, so neither it nor PR 4's conflicting prerequisite (also 3)
        # is checked out; master appears once though PRs 2, 3, and 6 need it.
        self.assertEqual(branches, [heads[2], master, heads[4], heads[6]])
        self.assertEqual(failures, [])

        copies.has_branch.return_value = True
        with (
            mock.patch.object(ccp, "head_branch", side_effect=lambda pr: heads[pr.number]),
            mock.patch.object(ccp, "base_branch", return_value=master),
        ):
            branches, _ = ccp.needed_branches(ccp.table_rows(DOCUMENT), copies)
        self.assertEqual(branches, [heads[2], heads[4], heads[6]])


if __name__ == "__main__":
    unittest.main()
