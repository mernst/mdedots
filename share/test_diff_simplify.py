"""Tests for diff-simplify.py."""

import importlib.machinery
import importlib.util
import sys
import types
import unittest
from pathlib import Path


def load_script() -> types.ModuleType:
    """Import diff-simplify.py, whose name is not a legal module name.

    Returns:
        The imported module.
    """
    path = Path(__file__).with_name("diff-simplify.py")
    loader = importlib.machinery.SourceFileLoader("diff_simplify", str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


diff_simplify = load_script()

TESTS_DIR = Path(__file__).with_name("diff-simplify-tests")


def hunk(*lines: str) -> str:
    """Return a diff that consists of the given lines.

    Args:
        *lines: the lines of the diff, without line terminators

    Returns:
        the diff
    """
    return "".join(line + "\n" for line in lines)


class GoalFileTest(unittest.TestCase):
    """Tests that compare the output for each NAME.diff file with NAME.diff.goal."""

    def test_goal_files(self):
        """The output for each test diff is its goal file."""
        diffs = sorted(TESTS_DIR.glob("*.diff"))
        self.assertTrue(diffs)
        for diff in diffs:
            with self.subTest(diff=diff.name):
                self.assertEqual(
                    diff_simplify.simplify_diff(diff.read_text(encoding="utf-8")),
                    diff.with_name(diff.name + ".goal").read_text(encoding="utf-8"),
                )


class ConsolidateTest(unittest.TestCase):
    """Tests for consolidating interleaved changes."""

    def test_merges_three_groups(self):
        """Three non-uniform change groups separated by short context are merged."""
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk(
                    "@@ -1,8 +1,9 @@",
                    "-a",
                    "+b",
                    "+c",
                    " d",
                    "+e",
                    "+f",
                    " g",
                    "-h",
                    "-i",
                    "-j",
                    "+k",
                    " }",
                    " last",
                )
            ),
            hunk(
                "@@ -1,8 +1,9 @@",
                "-a",
                "-d",
                "-g",
                "-h",
                "-i",
                "-j",
                "-}",
                "+b",
                "+c",
                "+d",
                "+e",
                "+f",
                "+g",
                "+k",
                "+}",
                " last",
            ),
        )

    def test_two_groups_are_not_merged(self):
        """Two change groups are not merged."""
        text = hunk("@@ -1,4 +1,4 @@", "-a", "+b", "+c", " d", "-e", "-f", "+g")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_uniform_groups_are_not_merged(self):
        """Line-for-line edits stay interleaved with their context."""
        text = hunk(
            "@@ -1,7 +1,7 @@",
            "-a",
            "+A",
            " b",
            "-c",
            "-d",
            "+C",
            "+D",
            " e",
            "-f",
            "-g",
            "+F",
            "+G",
        )
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_long_context_is_not_merged(self):
        """Context that is as long as an adjacent change group separates the groups."""
        text = hunk(
            "@@ -1,6 +1,10 @@",
            "-a",
            "+b",
            "+c",
            " d",
            " e",
            " f",
            "+g",
            "+h",
            " i",
            "-j",
            "+k",
            "+l",
        )
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_keeps_context_before_next_group(self):
        """Absorbing closing brackets leaves a context line before the next change group."""
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk(
                    "@@ -1,7 +1,11 @@",
                    "-a",
                    "+b",
                    "+c",
                    " d",
                    "+e",
                    "+f",
                    " g",
                    "-h",
                    "+i",
                    "+j",
                    " }",
                    " )",
                    "-k",
                    "+l",
                )
            ),
            hunk(
                "@@ -1,7 +1,11 @@",
                "-a",
                "-d",
                "-g",
                "-h",
                "-}",
                "+b",
                "+c",
                "+d",
                "+e",
                "+f",
                "+g",
                "+i",
                "+j",
                "+}",
                " )",
                "-k",
                "+l",
            ),
        )


class LengthenedLineTest(unittest.TestCase):
    """Tests for showing a lengthened line as an addition."""

    def test_appended_text(self):
        """A line to which text was appended shows only the appended text."""
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk("@@ -1,3 +1,3 @@", " a", "-# one two", "-three", "+# one two, and", "+four")
            ),
            hunk("@@ -1,3 +1,4 @@", " a", " # one two", "-three", "+         , and", "+four"),
        )

    def test_tabs_are_kept(self):
        """A tab in the common prefix stays a tab."""
        self.assertEqual(
            diff_simplify.simplify_diff(hunk("@@ -1 +1 @@", "-\tx = 1;", "+\tx = 1; // one")),
            hunk("@@ -1,1 +1,2 @@", " \tx = 1;", "+\t       // one"),
        )

    def test_blank_line_is_not_a_prefix(self):
        """A blank "-" line stays a "-" line."""
        text = hunk("@@ -1 +1 @@", "-  ", "+  x")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_partial_prefix(self):
        """A "+" line that starts with only part of the "-" line is unchanged."""
        text = hunk("@@ -1 +1 @@", "-    foo(a);", "+    foo(b);")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_only_first_lines(self):
        """Only the first "-" line and the first "+" line are compared."""
        text = hunk("@@ -1,2 +1,2 @@", "-a", "-bc", "+bcd", "+ef")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_no_newline_at_end_of_file(self):
        """A "-" line without a newline is not shown as context."""
        text = hunk(
            "@@ -1 +1 @@",
            "-abc",
            "\\ No newline at end of file",
            "+abcd",
            "\\ No newline at end of file",
        )
        self.assertEqual(diff_simplify.simplify_diff(text), text)


class ParseTest(unittest.TestCase):
    """Tests for parsing a diff."""

    def test_text_outside_hunks_is_unchanged(self):
        """Text that is not in a hunk, including text that looks like hunk lines, is unchanged."""
        text = hunk(
            "From: someone",
            "--- a/f",
            "+++ b/f",
            "@@ -1 +1 @@",
            "-x",
            "+xy",
            "--- a/g",
            "+++ b/g",
            "@@@ -1 -1 +1 @@@",
            "- x",
        )
        self.assertEqual(
            diff_simplify.simplify_diff(text),
            text.replace("@@ -1 +1 @@\n-x\n+xy\n", "@@ -1,1 +1,2 @@\n x\n+ y\n"),
        )

    def test_empty_context_line_without_space(self):
        """An empty context line from which a tool stripped the space is read as context."""
        self.assertEqual(
            diff_simplify.simplify_diff(hunk("@@ -1,2 +1,2 @@", "", "-a", "+b")),
            hunk("@@ -1,2 +1,2 @@", " ", "-a", "+b"),
        )

    def test_form_feed_is_not_a_line_break(self):
        """A form feed within a line does not end the line."""
        text = hunk("@@ -1 +1 @@", "-a\fb", "+c")
        self.assertEqual(diff_simplify.simplify_diff(text), text)


if __name__ == "__main__":
    unittest.main()
