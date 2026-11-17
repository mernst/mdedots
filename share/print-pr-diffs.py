#!/usr/bin/env python3

"""Make a printable PDF of the diff of each ready pull request in pr-status.md files.

Usage: print-pr-diffs.py [--dry-run] [PR-STATUS-FILE ...]

Each PR-STATUS-FILE is a file such as "pr-status.md" that the `pr-status` program
wrote; the default is "pr-status.md" in the current directory.  A pull request is
ready if it appears in an "Unblocked PRs" table of such a file, it has a working
copy directory (the table's "Directory" column), and its CI has not failed (its
"Unresolved threads" column lacks the red X that marks a failure).  A PR whose CI
is still running, or whose CI results GitHub did not report, counts as ready.

For each ready PR, this program does the following in the PR's working copy
directory:
 1. Run `git-pull-upstream` (alias `gpu`).
 2. Write the diff that `git-diff-upstream` prints to file N.diff, where N is the
    PR's number, as the alias `gduf` does.
 3. Simplify N.diff in place with the Emacs commands `diff-clean-tests` and
    `diff-clean-imports`.
 4. Run `difffontify` to convert N.diff to N.diff.pdf.
 5. Move N.diff.pdf to ~/sync/to-print/REPO-USERNAME-N.diff.pdf, where REPO is
    the PR's repository name as the table shows it, such as "checker-framework",
    and USERNAME is the GitHub username of the person who opened the PR, such as
    "mernst".  `gh` supplies USERNAME before step 1, so it must be logged in to
    github.com.
A PR is skipped, with a message, if any step fails or if the simplified diff is
empty; the other PRs are still processed.

With --dry-run, this program prints the ready PRs and the skipped PRs, but it runs
no command.

The exit status is 0 if every ready PR's PDF was moved to ~/sync/to-print/, and
1 otherwise.
"""

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import NamedTuple

# The directory that holds the PDFs to print.
TO_PRINT = Path.home() / "sync" / "to-print"

# The directory that holds this program, `difffontify`, and the sibling directory
# "emacs".  This program might be run via a symbolic link.
SHARE = Path(__file__).resolve().parent

# The directory that holds the Emacs library diff-clean.el.
EMACS_LISP = SHARE.parent / "emacs"

# The mark that `pr-status` writes in the "Unresolved threads" cell of a PR whose CI failed.
FAILED_MARK = "❌"

# Matches the heading of an "Unblocked PRs" table, such as "## Unblocked PRs (daikon)".
UNBLOCKED_HEADING_RE = re.compile(r"^## Unblocked PRs\b")

# Matches a link to a PR in a "PR" cell, such as
# "[checker-framework#6008](https://github.com/typetools/checker-framework/pull/6008)".
# Group "repo" is the repository name, group "number" is the PR's number, and group
# "url" is the PR's URL.
PR_LINK_RE = re.compile(r"^\[(?P<repo>[^]#]+)#(?P<number>\d+)\]\((?P<url>[^)]+)\)")

# Matches a working copy directory in a "Directory" cell, such as
# "`/home/me/research/types/checker-framework-fork-me-branch-foo/`".
# Group 1 is the directory.
DIRECTORY_RE = re.compile(r"`([^`]+)`")


class PullRequest(NamedTuple):
    """A pull request in an "Unblocked PRs" table of a pr-status.md file."""

    # The repository name, as the table shows it, such as "checker-framework".
    repo: str
    # The PR's number, such as 6008 in "checker-framework#6008".
    number: int
    # The PR's URL, such as "https://github.com/typetools/checker-framework/pull/6008".
    url: str
    # The PR's working copy directory, or None if the PR has none.
    directory: Path | None
    # True if the PR's CI failed.
    ci_failed: bool

    def label(self) -> str:
        """Return the PR's name.

        Returns:
            The PR's name, such as "checker-framework#6008".
        """
        return f"{self.repo}#{self.number}"


def split_row(line: str) -> list[str]:
    r"""Return the cells of the given Markdown table row, stripped of whitespace.

    Args:
        line: a Markdown table row, such as "| a | b \\| c |".

    Returns:
        The row's cells, such as ["a", "b \\| c"].  An escaped pipe character
        ("\\|") does not separate cells.
    """
    cells = re.split(r"(?<!\\)\|", line.strip())
    # The row starts and ends with "|", which yields an empty string at each end.
    return [cell.strip() for cell in cells[1:-1]]


def unblocked_prs(status_file: Path) -> list[PullRequest]:
    """Return the PRs in the "Unblocked PRs" tables of the given pr-status.md file.

    Args:
        status_file: a file that the `pr-status` program wrote.

    Returns:
        The PRs, in the order in which the file lists them.
    """
    result = []
    in_section = False
    header: list[str] | None = None
    for line in status_file.read_text(encoding="utf-8").splitlines():
        if line.startswith("## "):
            in_section = bool(UNBLOCKED_HEADING_RE.match(line))
            header = None
            continue
        if not in_section or not line.startswith("|"):
            continue
        cells = split_row(line)
        if header is None:
            header = cells
            continue
        if all(re.fullmatch(r":?-+:?", cell) for cell in cells):
            # The row that separates the header from the body.
            continue
        row = dict(zip(header, cells, strict=False))
        link = PR_LINK_RE.match(row.get("PR", ""))
        if not link:
            print(f"print-pr-diffs: {status_file}: cannot parse row: {line}", file=sys.stderr)
            continue
        directory = DIRECTORY_RE.search(row.get("Directory", ""))
        result.append(
            PullRequest(
                repo=link["repo"],
                number=int(link["number"]),
                url=link["url"],
                directory=Path(directory[1]) if directory else None,
                ci_failed=FAILED_MARK in row.get("Unresolved threads", ""),
            )
        )
    return result


def run(cmd: list[str], directory: Path, **kwargs) -> bool:
    """Run the given command in the given directory, and return True if it succeeds.

    Args:
        cmd: a command and its arguments.
        directory: the directory in which to run the command.
        kwargs: other arguments to `subprocess.run`.

    Returns:
        True if the command exits with status 0.
    """
    print(f"+ (cd {directory} && {' '.join(cmd)})", flush=True)
    return subprocess.run(cmd, cwd=directory, check=False, **kwargs).returncode == 0


def clean_diff(diff_file: Path) -> bool:
    """Simplify the given diff file in place with `diff-clean-tests` and `diff-clean-imports`.

    Args:
        diff_file: a file that contains a diff.

    Returns:
        True if Emacs succeeded.
    """
    form = (
        "(progn (setq make-backup-files nil)"
        f" (find-file {elisp_string(str(diff_file))})"
        " (diff-clean-tests) (diff-clean-imports) (save-buffer))"
    )
    return run(
        ["emacs", "-Q", "--batch", "-L", str(EMACS_LISP), "-l", "diff-clean", "--eval", form],
        diff_file.parent,
    )


def elisp_string(s: str) -> str:
    r"""Return an Emacs Lisp string literal whose value is the given string.

    Args:
        s: a string.

    Returns:
        An Emacs Lisp string literal, such as "a\\b" (6 characters) for the
        string a\b (3 characters).
    """
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def pr_author(pr: PullRequest) -> str | None:
    """Return the GitHub username of the person who opened the given PR.

    Args:
        pr: a PR.

    Returns:
        The username, such as "mernst", or None if `gh` cannot say.
    """
    result = subprocess.run(
        ["gh", "pr", "view", pr.url, "--json", "author", "--jq", ".author.login"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        print(f"print-pr-diffs: gh pr view {pr.url}: {result.stderr.strip()}", file=sys.stderr)
        return None
    return result.stdout.strip() or None


def print_pr(pr: PullRequest) -> str | None:
    """Make the PDF of the given PR's diff and move it to TO_PRINT.

    Args:
        pr: a PR that has a working copy directory.

    Returns:
        None if the PDF was moved to TO_PRINT, or else the reason that it was not,
        such as "git-pull-upstream failed".
    """
    assert pr.directory is not None
    directory = pr.directory
    if not directory.is_dir():
        return f"no directory {directory}"
    author = pr_author(pr)
    if author is None:
        return "gh cannot say who opened the PR"
    if not run(["git-pull-upstream"], directory):
        return "git-pull-upstream failed"
    diff_file = directory / f"{pr.number}.diff"
    with diff_file.open("w", encoding="utf-8") as out:
        if not run(["git-diff-upstream"], directory, stdout=out):
            return "git-diff-upstream failed"
    if not clean_diff(diff_file):
        return "Emacs diff-clean-tests or diff-clean-imports failed"
    if diff_file.stat().st_size == 0:
        return f"{diff_file} is empty after cleaning"
    if not run([str(SHARE / "difffontify"), diff_file.name], directory):
        return "difffontify failed"
    pdf_file = directory / f"{diff_file.name}.pdf"
    destination = TO_PRINT / f"{pr.repo}-{author}-{pdf_file.name}"
    shutil.move(pdf_file, destination)
    print(f"Wrote {destination}", flush=True)
    return None


def main() -> None:
    """Make PDFs of the diffs of the ready PRs in the pr-status.md files that argv names."""
    parser = argparse.ArgumentParser(
        description="Make a printable PDF of the diff of each ready PR in pr-status.md files."
    )
    parser.add_argument("--dry-run", action="store_true", help="list the PRs, but run no command")
    parser.add_argument(
        "status_files",
        metavar="PR-STATUS-FILE",
        nargs="*",
        type=Path,
        default=[Path("pr-status.md")],
        help='a file that `pr-status` wrote (default: "pr-status.md")',
    )
    args = parser.parse_args()

    # A list of (PR, reason it was skipped), and the ready PRs.
    skipped: list[tuple[PullRequest, str]] = []
    ready: list[PullRequest] = []
    for status_file in args.status_files:
        if not status_file.is_file():
            sys.exit(f"print-pr-diffs: no such file: {status_file}")
        for pr in unblocked_prs(status_file):
            if pr.ci_failed:
                skipped.append((pr, "CI failed"))
            elif pr.directory is None:
                skipped.append((pr, "no working copy directory"))
            else:
                ready.append(pr)

    if not args.dry_run:
        TO_PRINT.mkdir(parents=True, exist_ok=True)
    failed: list[tuple[PullRequest, str]] = []
    printed: list[PullRequest] = []
    for pr in ready:
        if args.dry_run:
            print(f"Ready: {pr.label()} in {pr.directory}")
            continue
        print(f"\n===== {pr.label()} =====", flush=True)
        reason = print_pr(pr)
        if reason is None:
            printed.append(pr)
        else:
            failed.append((pr, reason))

    print("\n===== Summary =====")
    if not args.dry_run:
        print(f"Moved to {TO_PRINT}: {', '.join(pr.label() for pr in printed) or '(none)'}")
    for pr, reason in failed:
        print(f"Failed: {pr.label()}: {reason}")
    for pr, reason in skipped:
        print(f"Skipped: {pr.label()}: {reason}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
