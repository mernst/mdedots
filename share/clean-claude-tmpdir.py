#!/usr/bin/env python3
"""Delete stale Claude Code temporary directories.

Usage:
  clean-claude-tmpdir.py [--dry-run] [--scratchpad-days DAYS] [TMPDIR ...]

TMPDIR defaults to each of /tmp/claude-$UID and $CLAUDE_CODE_TMPDIR/claude-$UID
that exists.  Claude Code names each subdirectory of a TMPDIR
after a project directory, replacing every non-alphanumeric character of the
project directory's absolute path by "-".  For example,
/home/mernst/.emacs.d becomes -home-mernst--emacs-d.  Each such project
subdirectory contains one subdirectory per session, which may contain a
scratchpad directory.

This script deletes:
 * each project subdirectory that does not correspond to an existing directory,
   and
 * each session's scratchpad directory, if nothing in it has changed for DAYS
   days (default 3).
It does not touch other files or subdirectories of a TMPDIR.

Because the encoding is lossy, the script decodes a name by walking the file
system from "/":  at each directory, it descends into every subdirectory whose
encoded name matches the next part of the name.

Options:
  --dry-run, -n           report the directories that would be deleted, but do
                          not delete them
  --scratchpad-days DAYS  delete scratchpads in which nothing has changed for
                          this many days
"""

import argparse
import functools
import os
import pathlib
import re
import shutil
import sys
import time

# Claude Code truncates encoded names that are longer than this, and appends a hash.
MAX_ENCODED_LENGTH = 200


def encode(name: str) -> str:
    """Encode a file name as Claude Code encodes a path.

    Returns:
        The name, with each non-alphanumeric character replaced by "-".
    """
    return re.sub(r"[^a-zA-Z0-9]", "-", name)


@functools.cache
def subdirectories(directory: pathlib.Path) -> list[tuple[str, pathlib.Path]] | None:
    """List the subdirectories of a directory.

    Returns:
        The encoded name and the path of each subdirectory, or None if the directory
        cannot be read.
    """
    try:
        return [(encode(p.name), p) for p in directory.iterdir() if p.is_dir()]
    except OSError:
        return None


def exists_under(directory: pathlib.Path, rest: str, truncated: bool) -> bool:
    """Determine whether some directory under `directory` has encoded relative path `rest`.

    If `truncated` is true, `rest` need only be a prefix of the encoded relative path.
    An unreadable directory is assumed to contain a match.

    Returns:
        True if such a directory exists.
    """
    if rest == "":
        return True
    subdirs = subdirectories(directory)
    if subdirs is None:
        return True
    for encoded, path in subdirs:
        if truncated and encoded.startswith(rest):
            return True
        if rest == encoded or (
            rest.startswith(encoded + "-")
            and exists_under(path, rest[len(encoded) + 1 :], truncated)
        ):
            return True
    return False


def corresponds_to_directory(name: str) -> bool:
    """Determine whether a name is the encoding of an existing directory.

    Returns:
        True if the name is the encoding of an existing directory.
    """
    truncated = len(name) > MAX_ENCODED_LENGTH
    if truncated:
        name = name[:MAX_ENCODED_LENGTH]
    return exists_under(pathlib.Path("/"), name[1:], truncated)


def changed_since(directory: pathlib.Path, cutoff: float) -> bool:
    """Determine whether anything in a directory has changed since a time.

    A file's change time is the later of its modification time and its status
    change time, so that a file extracted from an archive counts as changed when
    it was extracted.

    Returns:
        True if the directory or any file under it changed after `cutoff`, which is
        in seconds since the epoch.
    """
    for dirpath, dirnames, filenames in os.walk(directory):
        for name in [".", *dirnames, *filenames]:
            try:
                st = pathlib.Path(dirpath, name).lstat()
            except OSError:
                continue
            if max(st.st_mtime, st.st_ctime) > cutoff:
                return True
    return False


def delete(path: pathlib.Path, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, a directory.

    Returns:
        False if the deletion failed.
    """
    if dry_run:
        print(f"Would delete {path}")
        return True
    print(f"Deleting {path}")
    try:
        shutil.rmtree(path)
    except OSError as e:
        print(f"Could not delete {path}: {e}", file=sys.stderr)
        return False
    return True


def main() -> None:
    """Delete, or with --dry-run report, the stale directories."""
    parser = argparse.ArgumentParser(description="Delete stale Claude Code temporary directories.")
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="report the directories that would be deleted, but do not delete them",
    )
    parser.add_argument(
        "--scratchpad-days",
        type=float,
        default=3,
        metavar="DAYS",
        help="delete scratchpads in which nothing has changed for this many days"
        " (default: %(default)s)",
    )
    parser.add_argument(
        "tmpdirs",
        nargs="*",
        type=pathlib.Path,
        metavar="TMPDIR",
        help="a Claude Code temporary directory (default: each of /tmp/claude-$UID and"
        " $CLAUDE_CODE_TMPDIR/claude-$UID that exists)",
    )
    args = parser.parse_args()

    if args.tmpdirs:
        tmpdirs = args.tmpdirs
        for tmpdir in tmpdirs:
            if not tmpdir.is_dir():
                sys.exit(f"{tmpdir} is not a directory")
    else:
        bases = {pathlib.Path("/tmp")}
        if os.environ.get("CLAUDE_CODE_TMPDIR"):
            bases.add(pathlib.Path(os.environ["CLAUDE_CODE_TMPDIR"]).resolve())
        tmpdirs = [
            tmpdir for base in sorted(bases) if (tmpdir := base / f"claude-{os.getuid()}").is_dir()
        ]

    cutoff = time.time() - args.scratchpad_days * 24 * 60 * 60
    ok = True
    for tmpdir in tmpdirs:
        for project in sorted(tmpdir.iterdir()):
            if not project.name.startswith("-") or project.is_symlink() or not project.is_dir():
                continue
            if not corresponds_to_directory(project.name):
                ok &= delete(project, args.dry_run)
                continue
            for scratchpad in sorted(project.glob("*/scratchpad")):
                if (
                    scratchpad.is_dir()
                    and not scratchpad.is_symlink()
                    and not changed_since(scratchpad, cutoff)
                ):
                    ok &= delete(scratchpad, args.dry_run)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
