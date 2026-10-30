"""Tests for diff-simplify.py."""

import sys
import unittest
from pathlib import Path

SCRIPT_DIRECTORY = Path(__file__).parent
if str(SCRIPT_DIRECTORY) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIRECTORY))

from script_loader import load_script  # ruff: ignore[module-import-not-at-top-of-file]

diff_simplify = load_script("diff-simplify.py")

TESTS_DIR = SCRIPT_DIRECTORY / "diff-simplify-tests"


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

    def test_absorbs_all_closing_lines_at_end_of_hunk(self):
        """A merged region at the end of a hunk absorbs every closing line that follows it."""
        second_hunk = hunk("@@ -20,6 +17,4 @@", "-k1", "-k2", "+l", " m", "-n1", "-n2", "+o", " }")
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk(
                    "@@ -1,10 +1,7 @@",
                    "-a1",
                    "-a2",
                    "+b",
                    " c",
                    "-d1",
                    "-d2",
                    "+e",
                    " f",
                    "-g1",
                    "-g2",
                    "+h",
                    "   }",
                    " }",
                )
                + second_hunk
            ),
            hunk(
                "@@ -1,10 +1,7 @@",
                "-a1",
                "-a2",
                "-c",
                "-d1",
                "-d2",
                "-f",
                "-g1",
                "-g2",
                "-  }",
                "-}",
                "+b",
                "+c",
                "+e",
                "+f",
                "+h",
                "+  }",
                "+}",
            )
            + second_hunk,
        )

    def test_merged_region_has_no_lengthened_line(self):
        """A merged region's first old line and first new line are not compared."""
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk(
                    "@@ -1,6 +1,6 @@",
                    "+x = 1;",
                    "+y",
                    " x",
                    "-p",
                    "-q",
                    "+r",
                    " s",
                    "-t",
                    "-u",
                    "+v",
                )
            ),
            hunk(
                "@@ -1,6 +1,6 @@",
                "-x",
                "-p",
                "-q",
                "-s",
                "-t",
                "-u",
                "+x = 1;",
                "+y",
                "+x",
                "+r",
                "+s",
                "+v",
            ),
        )

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

    def test_carriage_return(self):
        """A trailing carriage return does not prevent the simplification."""
        self.assertEqual(
            diff_simplify.simplify_diff(hunk("@@ -1 +1 @@", "-abc\r", "+abcd\r")),
            hunk("@@ -1,1 +1,2 @@", " abc\r", "+   d\r"),
        )

    def test_changed_line_terminator(self):
        """A line to which only a carriage return was appended is unchanged."""
        text = hunk("@@ -1 +1 @@", "-abc", "+abc\r")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_removed_carriage_return(self):
        """A line from which a carriage return was removed is unchanged."""
        text = hunk("@@ -1 +1 @@", "-abc\r", "+abcd")
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

    def test_empty_crlf_context_line_without_space(self):
        """In a CRLF diff, an empty context line from which a tool stripped the space is context."""
        self.assertEqual(
            diff_simplify.simplify_diff(hunk("@@ -1,2 +1,2 @@\r", "\r", "-a\r", "+ab\r")),
            hunk("@@ -1,2 +1,3 @@\r", " \r", " a\r", "+ b\r"),
        )

    def test_overstated_hunk_header(self):
        """A hunk whose header claims too many lines ends at the next file's header."""
        next_file = hunk("--- a/g", "+++ b/g", "@@ -1 +1 @@", "-p", "+q")
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk("--- a/f", "+++ b/f", "@@ -1,5 +1,5 @@", "-x", "+xy", " c") + next_file
            ),
            hunk("--- a/f", "+++ b/f", "@@ -1,2 +1,3 @@", " x", "+ y", " c") + next_file,
        )

    def test_hunk_header_overstated_by_one_line(self):
        """A hunk whose header claims one old and one new line too many ends at a file header."""
        next_file = hunk("--- a/g", "+++ b/g", "@@ -1 +1 @@", "-p", "+q")
        self.assertEqual(
            diff_simplify.simplify_diff(
                hunk("--- a/f", "+++ b/f", "@@ -1,3 +1,3 @@", "-x", "+xy", " c") + next_file
            ),
            hunk("--- a/f", "+++ b/f", "@@ -1,2 +1,3 @@", " x", "+ y", " c") + next_file,
        )

    def test_overstated_hunk_header_before_separator(self):
        """A hunk whose header claims too many lines ends at a blank line or a signature."""
        for separator in ["", "-- "]:
            with self.subTest(separator=separator):
                self.assertEqual(
                    diff_simplify.simplify_diff(
                        hunk("@@ -1,3 +1,3 @@", "-a", "+ab", separator, "2.43.0")
                    ),
                    hunk("@@ -1,1 +1,2 @@", " a", "+ b", separator, "2.43.0"),
                )

    def test_hunk_ends_with_lines_like_separator(self):
        """A hunk can end with a blank line or a "-- " line."""
        for header, last, simplified_last in [
            ("@@ -1,2 +1,1 @@", "", " "),
            ("@@ -1,2 +1,0 @@", "-- ", "-- "),
        ]:
            with self.subTest(last=last):
                self.assertEqual(
                    diff_simplify.simplify_diff(hunk(header, "-a", last, "2.43.0")),
                    hunk(header, "-a", simplified_last, "2.43.0"),
                )

    def test_hunk_ends_with_lines_like_file_header(self):
        """A hunk can end with a "-" line and a "+" line that look like a file's header."""
        text = hunk("@@ -1,2 +1,2 @@", " a", "--- x", "+++ y", "@@ -9 +9 @@", "-p", "+q")
        self.assertEqual(diff_simplify.simplify_diff(text), text)

    def test_form_feed_is_not_a_line_break(self):
        """A form feed within a line does not end the line."""
        text = hunk("@@ -1 +1 @@", "-a\fb", "+c")
        self.assertEqual(diff_simplify.simplify_diff(text), text)


if __name__ == "__main__":
    unittest.main()
