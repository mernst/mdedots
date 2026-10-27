#!/usr/bin/env python3
"""Delete Claude Code temporary directories whose project directory no longer exists.

Usage:
  clean-claude-tmpdir.py [--dry-run] [TMPDIR]

TMPDIR defaults to /tmp/claude-$UID.  Claude Code names each subdirectory of it
after a project directory, replacing every non-alphanumeric character of the
project directory's absolute path by "-".  For example,
/home/mernst/.emacs.d becomes -home-mernst--emacs-d.  This script deletes each
subdirectory of TMPDIR whose name starts with "-" and that does not correspond
to an existing directory.  It does not touch other files or subdirectories of
TMPDIR.

Because the encoding is lossy, the script decodes a name by walking the file
system from "/":  at each directory, it descends into every subdirectory whose
encoded name matches the next part of the name.

Options:
  --dry-run, -n   report the subdirectories that would be deleted, but do not
                  delete them
"""

import argparse
import functools
import os
import pathlib
import re
import shutil
import sys

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


def main() -> None:
    """Delete, or with --dry-run report, the stale subdirectories."""
    parser = argparse.ArgumentParser(
        description="Delete Claude Code temporary directories whose project directory"
        " no longer exists."
    )
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="report the subdirectories that would be deleted, but do not delete them",
    )
    parser.add_argument(
        "tmpdir",
        nargs="?",
        type=pathlib.Path,
        default=pathlib.Path(f"/tmp/claude-{os.getuid()}"),
        help="the Claude Code temporary directory (default: %(default)s)",
    )
    args = parser.parse_args()

    if not args.tmpdir.is_dir():
        sys.exit(f"{args.tmpdir} is not a directory")

    status = 0
    for path in sorted(args.tmpdir.iterdir()):
        if (
            not path.name.startswith("-")
            or path.is_symlink()
            or not path.is_dir()
            or corresponds_to_directory(path.name)
        ):
            continue
        if args.dry_run:
            print(f"Would delete {path}")
        else:
            print(f"Deleting {path}")
            try:
                shutil.rmtree(path)
            except OSError as e:
                print(f"Could not delete {path}: {e}", file=sys.stderr)
                status = 1
    sys.exit(status)


if __name__ == "__main__":
    main()
