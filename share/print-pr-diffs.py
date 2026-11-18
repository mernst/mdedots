#!/usr/bin/env python3

"""Make a printable PDF of the diff of each ready pull request that `pr-status` reports.

Usage: print-pr-diffs.py [--dry-run] [DIR ...]

Each DIR is a directory in which the `pr-status` program runs, such as
~/research/types:  it holds the working copy directories and pr-status.md.  The
default is the current directory.  This program reads the unblocked PRs that
`pr-status --print-unblocked-prs` prints, which are the ones in the "Unblocked
PRs" tables of pr-status.md as of the most recent run of `pr-status`.  An unblocked
PR is ready if it has a working copy directory and its CI has not failed.  A PR
whose CI is still running, or whose CI results GitHub did not report, counts as
ready.

For each ready PR, this program does the following in the PR's working copy
directory (the first one, if it has several):
 1. Write the diff that `git-diff-upstream` prints to file N.diff, where N is
    the PR's number, as the alias `gduf` does.
 2. Simplify N.diff in place with the Emacs commands `diff-clean-tests` and
    `diff-clean-imports`.
 3. Run `difffontify` to convert N.diff to N.diff.pdf.
 4. Move N.diff.pdf to ~/tmp/REPO-USERNAME-N.diff.pdf, where REPO is
    the repository's key in pr-status's config, such as "checker-framework", and
    USERNAME is the GitHub username of the person who opened the PR, such as
    "mernst", with each "/" replaced by "-", as in "app-dependabot".  `gh`
    supplies USERNAME before step 1, so it must be logged in to github.com.
This program does not merge upstream changes into the working copy directory, so
the diff is of the working tree as it is.
A PR is skipped, with a message, if any step fails or if the simplified diff is
empty; the other PRs are still processed.

With --dry-run, this program prints the ready PRs and the skipped PRs, but it runs
no command other than `pr-status --print-unblocked-prs`.

If `pr-status` does not report the PRs of a DIR, this program prints a message and
goes on to the next DIR.

The exit status is 0 if `pr-status` reported the PRs of every DIR and every ready
PR's PDF was moved to ~/tmp/, and 1 otherwise.
"""

import argparse
import json
import shlex
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, NamedTuple

# The directory to which this program moves the PDFs.
OUTPUT_DIR = Path.home() / "tmp"

# The directory that holds this program, `difffontify`, and the sibling directory
# "emacs".  This program might be run via a symbolic link.
SHARE = Path(__file__).resolve().parent

# The directory that holds the Emacs library diff-clean.el.
EMACS_LISP = SHARE.parent / "emacs"


class PullRequest(NamedTuple):
    """An unblocked pull request that `pr-status --print-unblocked-prs` reports."""

    # The repository's key in pr-status's config, such as "checker-framework".
    repo: str
    # The PR's number, such as 6008 in "checker-framework#6008".
    number: int
    # The PR's URL, such as "https://github.com/typetools/checker-framework/pull/6008".
    url: str
    # The PR's first working copy directory, or None if the PR has none.
    directory: Path | None
    # True if the PR's CI failed.
    ci_failed: bool

    @classmethod
    def from_json(cls, pr: dict[str, Any]) -> "PullRequest":
        """Return the PullRequest that the given JSON object describes.

        Args:
            pr: a PR, as a dict parsed from the JSON that `pr-status
                --print-unblocked-prs` prints, with keys such as "repo", "number",
                "url", "directories", and "ci".

        Returns:
            The PullRequest.
        """
        directories = pr["directories"]
        return cls(
            repo=pr["repo"],
            number=pr["number"],
            url=pr["url"],
            directory=Path(directories[0]) if directories else None,
            ci_failed=pr["ci"] == "failed",
        )

    def label(self) -> str:
        """Return the PR's name.

        Returns:
            The PR's name, such as "checker-framework#6008".
        """
        return f"{self.repo}#{self.number}"


def unblocked_prs(directory: Path) -> list[PullRequest] | str:
    """Return the unblocked PRs that `pr-status` reports for the given directory.

    Args:
        directory: a directory in which `pr-status` runs, such as ~/research/types.

    Returns:
        The PRs, in the order of the "Unblocked PRs" tables of the directory's pr-status.md,
        or else the reason that `pr-status` did not report them, such as "pr-status:
        --print-unblocked-prs: .pr-status-snapshot.json does not record the unblocked PRs,
        ...".
    """
    cmd = ["pr-status", "-C", str(directory), "--print-unblocked-prs"]
    command = shlex.join(cmd)
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=False)
    except OSError as error:
        return f"{command}: {error}"
    if result.returncode != 0:
        return result.stderr.strip() or f"{command} failed"
    try:
        return [PullRequest.from_json(pr) for pr in json.loads(result.stdout)]
    except (ValueError, KeyError, TypeError) as error:
        return f"{command}: unexpected output: {error!r}"


def run(cmd: list[str], directory: Path, **kwargs) -> bool:
    """Run the given command in the given directory, and return True if it succeeds.

    Args:
        cmd: a command and its arguments.
        directory: the directory in which to run the command.
        kwargs: other arguments to `subprocess.run`.

    Returns:
        True if the command exits with status 0.
    """
    print(f"+ (cd {shlex.quote(str(directory))} && {shlex.join(cmd)})", flush=True)
    return subprocess.run(cmd, cwd=directory, check=False, **kwargs).returncode == 0


def clean_diff(diff_file: Path) -> bool:
    """Simplify the given diff file in place with `diff-clean-tests` and `diff-clean-imports`.

    Args:
        diff_file: a file that contains a diff.

    Returns:
        True if Emacs succeeded.
    """
    return run(
        [
            "emacs",
            "-Q",
            "--batch",
            "-L",
            str(EMACS_LISP),
            "-l",
            "diff-clean",
            "-f",
            "diff-clean-batch-tests-and-imports",
            diff_file.name,
        ],
        diff_file.parent,
    )


def pr_author(pr: PullRequest) -> str | None:
    """Return the GitHub username of the person who opened the given PR.

    Args:
        pr: a PR.

    Returns:
        The username, such as "mernst" or "app/dependabot", or None if `gh` cannot say.
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
    """Make the PDF of the given PR's diff and move it to OUTPUT_DIR.

    Args:
        pr: a PR that has a working copy directory.

    Returns:
        None if the PDF was moved to OUTPUT_DIR, or else the reason that it was not,
        such as "git-diff-upstream failed".
    """
    assert pr.directory is not None
    directory = pr.directory
    if not directory.is_dir():
        return f"no directory {directory}"
    author = pr_author(pr)
    if author is None:
        return "gh cannot say who opened the PR"
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
    destination = OUTPUT_DIR / f"{pr.repo}-{author.replace('/', '-')}-{pdf_file.name}"
    shutil.move(pdf_file, destination)
    print(f"Wrote {destination}", flush=True)
    return None


def main() -> None:
    """Make PDFs of the diffs of the ready PRs of the directories that argv names."""
    parser = argparse.ArgumentParser(
        description="Make a printable PDF of the diff of each ready PR that `pr-status` reports."
    )
    parser.add_argument("--dry-run", action="store_true", help="list the PRs; run only pr-status")
    parser.add_argument(
        "directories",
        metavar="DIR",
        nargs="*",
        type=Path,
        default=[Path()],
        help="a directory in which `pr-status` runs (default: the current directory)",
    )
    args = parser.parse_args()

    # A list of (PR, reason it was skipped), and the ready PRs.
    skipped: list[tuple[PullRequest, str]] = []
    ready: list[PullRequest] = []
    # A list of (directory, reason that `pr-status` did not report its PRs).
    failed_directories: list[tuple[Path, str]] = []
    for directory in args.directories:
        prs = unblocked_prs(directory)
        if isinstance(prs, str):
            print(f"print-pr-diffs: {prs}", file=sys.stderr)
            failed_directories.append((directory, prs))
            continue
        for pr in prs:
            if pr.ci_failed:
                skipped.append((pr, "CI failed"))
            elif pr.directory is None:
                skipped.append((pr, "no working copy directory"))
            else:
                ready.append(pr)

    if not args.dry_run:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    failed: list[tuple[PullRequest, str]] = []
    printed: list[PullRequest] = []
    for pr in ready:
        if args.dry_run:
            print(f"Ready: {pr.label()} in {pr.directory}")
            continue
        print(f"\n===== {pr.label()} =====", flush=True)
        try:
            reason = print_pr(pr)
        except OSError as error:
            reason = str(error)
        if reason is None:
            printed.append(pr)
        else:
            failed.append((pr, reason))

    print("\n===== Summary =====")
    if not args.dry_run:
        print(f"Moved to {OUTPUT_DIR}: {', '.join(pr.label() for pr in printed) or '(none)'}")
    for directory, reason in failed_directories:
        print(f"Failed: {directory}: {reason}")
    for pr, reason in failed:
        print(f"Failed: {pr.label()}: {reason}")
    for pr, reason in skipped:
        print(f"Skipped: {pr.label()}: {reason}")
    sys.exit(1 if failed or failed_directories else 0)


if __name__ == "__main__":
    main()
