#!/usr/bin/env python3

"""Make a printable PDF of the diff of each ready pull request that `pr-status` reports.

Usage: print-pr-diffs.py [--dry-run] [--no-pr-status] [DIR ...]

Each DIR is a directory in which the `pr-status` program runs, such as
~/research/types:  it holds the working copy directories and pr-status.md.  The
default is the current directory.  In each DIR, this program first runs
`pr-status --previous-arguments`, which regenerates pr-status.md (keeping the
highlights of the previous run of `pr-status`) and records the PRs that are open
now.  Then it reads the unblocked PRs that `pr-status --print-unblocked-prs`
prints, which are the ones in the "Unblocked PRs" tables of pr-status.md.  An
unblocked PR depends on no other open PR.  An unblocked PR is ready if it has a
working copy directory and its CI has not failed.  A PR whose CI is still
running, or whose CI results GitHub did not report, counts as ready.

If `pr-status` fails after recording the PRs, as it does when it cannot update a
PR's description, this program processes the PRs that it recorded.  If
`pr-status` fails without recording them, this program skips the DIR.

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
    "mernst", with each "/" replaced by "-", as in "app-dependabot".
This program does not merge upstream changes into the working copy directory, so
the diff is of the working tree as it is.
A PR is skipped, with a message, if any step fails or if the simplified diff is
empty; the other PRs are still processed.

With --dry-run, this program prints the ready PRs and the skipped PRs, but it runs
no command other than `pr-status`.  It runs `pr-status` with `--no-documents`, so
`pr-status` updates PR descriptions, renames working copy directories, and records
the PRs, but it does not regenerate pr-status.md.

With --no-pr-status, this program does not run `pr-status` other than as
`pr-status --print-unblocked-prs`, so it uses the PRs that the most recent run of
`pr-status` recorded.

If `pr-status` does not report the PRs of a DIR, this program prints a message and
goes on to the next DIR.

The exit status is 0 if `pr-status` succeeded in every DIR and every ready PR's PDF
was moved to ~/tmp/, and 1 otherwise.
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

# The file, in a directory in which `pr-status` runs, where `pr-status` records the
# PRs that `pr-status --print-unblocked-prs` prints.
PR_STATUS_SNAPSHOT = ".pr-status-snapshot.json"


class PrStatusOutcome(NamedTuple):
    """The outcome of a run of `pr-status` that records the open PRs."""

    # True if the run recorded the PRs, even if it failed afterward.
    recorded: bool
    # The reason that the run failed, such as "pr-status -C ~/research/types
    # --previous-arguments failed", or None if it succeeded.
    failure: str | None


class PullRequest(NamedTuple):
    """An unblocked pull request that `pr-status --print-unblocked-prs` reports."""

    # The repository's key in pr-status's config, such as "checker-framework".
    repo: str
    # The PR's number, such as 6008 in "checker-framework#6008".
    number: int
    # The PR's URL, such as "https://github.com/typetools/checker-framework/pull/6008".
    url: str
    # The GitHub username of the person who opened the PR, such as "mernst" or
    # "app/dependabot".
    author: str
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
                "url", "author", "directories", and "ci".

        Returns:
            The PullRequest.
        """
        directories = pr["directories"]
        return cls(
            repo=pr["repo"],
            number=pr["number"],
            url=pr["url"],
            author=pr["author"],
            directory=Path(directories[0]) if directories else None,
            ci_failed=pr["ci"] == "failed",
        )

    def label(self) -> str:
        """Return the PR's name.

        Returns:
            The PR's name, such as "checker-framework#6008".
        """
        return f"{self.repo}#{self.number}"


def read_snapshot(directory: Path) -> bytes | None:
    """Return the contents of the given directory's `pr-status` snapshot.

    Args:
        directory: a directory in which `pr-status` runs, such as ~/research/types.

    Returns:
        The contents of the directory's PR_STATUS_SNAPSHOT, or None if it cannot be read.
    """
    try:
        return (directory / PR_STATUS_SNAPSHOT).read_bytes()
    except OSError:
        return None


def run_pr_status(directory: Path, dry_run: bool) -> PrStatusOutcome:
    """Run `pr-status` in the given directory, so that it records the PRs that are open now.

    Args:
        directory: a directory in which `pr-status` runs, such as ~/research/types.
        dry_run: True if `pr-status` should not regenerate pr-status.md.

    Returns:
        The outcome of the run.  The run recorded the PRs if it succeeded or if it
        changed the directory's PR_STATUS_SNAPSHOT.
    """
    cmd = ["pr-status", "-C", str(directory), "--previous-arguments"]
    if dry_run:
        cmd.append("--no-documents")
    command = shlex.join(cmd)
    print(f"+ {command}", flush=True)
    old_snapshot = read_snapshot(directory)
    try:
        result = subprocess.run(cmd, stdin=subprocess.DEVNULL, check=False)
    except OSError as error:
        return PrStatusOutcome(recorded=False, failure=f"{command}: {error}")
    if result.returncode == 0:
        return PrStatusOutcome(recorded=True, failure=None)
    new_snapshot = read_snapshot(directory)
    recorded = new_snapshot is not None and new_snapshot != old_snapshot
    return PrStatusOutcome(recorded=recorded, failure=f"{command} failed")


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
    except KeyError as error:
        return (
            f"{command}: the snapshot records no {error}, because an older version of"
            " pr-status wrote it; run pr-status to rewrite it"
        )
    except (ValueError, TypeError) as error:
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
    destination = OUTPUT_DIR / f"{pr.repo}-{pr.author.replace('/', '-')}-{pdf_file.name}"
    shutil.move(pdf_file, destination)
    print(f"Wrote {destination}", flush=True)
    return None


def main() -> None:
    """Make PDFs of the diffs of the ready PRs of the directories that argv names."""
    parser = argparse.ArgumentParser(
        description="Make a printable PDF of the diff of each ready PR that `pr-status` reports."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="list the PRs; run only pr-status, which does not regenerate pr-status.md",
    )
    parser.add_argument(
        "--no-pr-status",
        action="store_true",
        help="use the PRs that the most recent run of pr-status recorded, rather than running it",
    )
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
    # A list of (directory, reason that `pr-status` failed or did not report its PRs).
    failed_directories: list[tuple[Path, str]] = []
    for directory in args.directories:
        if not args.no_pr_status:
            outcome = run_pr_status(directory, args.dry_run)
            if outcome.failure is not None:
                failed_directories.append((directory, outcome.failure))
                if not outcome.recorded:
                    print(f"print-pr-diffs: {outcome.failure}", file=sys.stderr)
                    continue
                print(
                    f"print-pr-diffs: {outcome.failure}, but it recorded the PRs; continuing",
                    file=sys.stderr,
                )
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
