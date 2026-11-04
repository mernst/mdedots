#!/usr/bin/env python3
"""Delete stale Claude Code temporary directories and jdtls workspaces.

Usage:
  clean-tmpdir.py [--dry-run] [--days DAYS] [TMPDIR ...]

TMPDIR defaults to each of /tmp/claude-$UID, $CLAUDE_CODE_TMPDIR/claude-$UID,
and ~/.cache/jdtls (on macOS, ~/Library/Caches/jdtls) that exists.  The script
cleans two kinds of subdirectory of a TMPDIR:
 * A Claude Code project subdirectory, whose name is the absolute path of a
   project directory with every non-alphanumeric character replaced by "-".
   For example, /home/mernst/.emacs.d becomes -home-mernst--emacs-d.  A project
   subdirectory corresponds to its project directory.  It contains one
   subdirectory per session, which may contain a scratchpad directory.
 * A jdtls workspace, whose name starts with "jdtls-".  jdtls creates one
   workspace per basename of a directory in which jdtls was started, so
   directories with the same basename share a workspace.  A workspace corresponds
   to the files and directories that it records as covered.  A workspace that
   records none is treated as corresponding to an existing directory.

This script deletes:
 * each project subdirectory and each jdtls workspace that corresponds to no
   existing file or directory, and
 * each session's scratchpad directory and each jdtls workspace in which nothing
   has changed for DAYS days (default 7).
It does not touch other files or directories.

Because the Claude Code encoding is lossy, the script decodes a project
subdirectory's name by walking the file system from "/":  at each directory, it
descends into every subdirectory whose encoded name matches the next part of the
name.

Options:
  --dry-run, -n           report the directories that would be deleted, but do
                          not delete them
  --days DAYS             delete scratchpads and jdtls workspaces in which
                          nothing has changed for this many days
"""

import argparse
import functools
import os
import pathlib
import platform
import re
import shutil
import sys
import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET

# Claude Code truncates encoded names that are longer than this, and appends a hash.
MAX_ENCODED_LENGTH = 200

# The directory that holds the jdtls workspaces, as the jdtls launcher jdtls.py chooses it.
JDTLS_CACHE = (
    pathlib.Path.home() / "Library" / "Caches" / "jdtls"
    if platform.system() == "Darwin"
    else pathlib.Path.home() / ".cache" / "jdtls"
)


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
    for dirpath, dirnames, filenames in directory.walk():
        for name in [".", *dirnames, *filenames]:
            try:
                st = (dirpath / name).lstat()
            except OSError:
                continue
            if max(st.st_mtime, st.st_ctime) > cutoff:
                return True
    return False


def path_from_file_uri(uri: str) -> pathlib.Path | None:
    """Convert a URI to a path.

    Returns:
        The path that `uri`, such as "file:/home/me/my%20project", names, or None
        if `uri` is not a "file:" URI.
    """
    parsed = urllib.parse.urlparse(uri)
    if parsed.scheme != "file":
        return None
    return pathlib.Path(urllib.request.url2pathname(parsed.path))


def jdtls_covered_paths(workspace: pathlib.Path) -> list[pathlib.Path]:
    """Return the files and directories that the given jdtls workspace covers.

    These are the root of each project that jdtls imported, which jdtls records in
    a binary ".location" file, and each file or directory that a project in the
    workspace links to, which jdtls records in that project's ".project" file.
    Unreadable records are ignored.

    Args:
        workspace: a jdtls workspace, such as
            ~/.cache/jdtls/jdtls-0a422631bbbc5fff4218961dc4439516f8308e91.

    Returns:
        The covered files and directories, which need not exist.
    """
    result = []
    for location in workspace.glob(
        ".metadata/.plugins/org.eclipse.core.resources/.projects/*/.location"
    ):
        # The file contains a string written by Java's DataOutputStream.writeUTF:  a
        # 2-byte big-endian length, followed by "URI//" and the project root's URI.
        try:
            data = location.read_bytes()
        except OSError:
            continue
        start = data.find(b"URI//")
        if start < 2:
            continue
        end = start + int.from_bytes(data[start - 2 : start])
        path = path_from_file_uri(data[start + len(b"URI//") : end].decode(errors="replace"))
        if path is not None:
            result.append(path)
    for project in workspace.glob("*/.project"):
        try:
            links = ET.parse(project).getroot().iterfind("linkedResources/link")
            for link in links:
                if text := link.findtext("location"):
                    result.append(pathlib.Path(text))
                elif (text := link.findtext("locationURI")) and (
                    path := path_from_file_uri(text)
                ) is not None:
                    result.append(path)
        except (OSError, ET.ParseError):
            continue
    return result


def exists(path: pathlib.Path) -> bool:
    """Determine whether a file or directory exists.

    A path whose existence cannot be determined, for example because a directory
    on it cannot be read, is assumed to exist.

    Returns:
        True if `path` exists.
    """
    try:
        path.stat()
    except (FileNotFoundError, NotADirectoryError):
        return False
    except OSError:
        return True
    return True


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


def clean_claude_project(project: pathlib.Path, cutoff: float, dry_run: bool) -> bool:
    """Clean the given Claude Code project subdirectory.

    Delete, or with `dry_run` report, the project subdirectory if it corresponds to no
    existing directory, and otherwise each of its stale scratchpads.

    Args:
        project: a Claude Code project subdirectory, such as
            /tmp/claude-1000/-home-mernst--emacs-d.
        cutoff: a time, in seconds since the epoch; a scratchpad in which nothing has
            changed since the cutoff is stale.
        dry_run: true if the directories should be reported, but not deleted.

    Returns:
        False if a deletion failed.
    """
    if not corresponds_to_directory(project.name):
        return delete(project, dry_run)
    ok = True
    for scratchpad in sorted(project.glob("*/scratchpad")):
        if (
            scratchpad.is_dir()
            and not scratchpad.is_symlink()
            and not changed_since(scratchpad, cutoff)
        ):
            ok &= delete(scratchpad, dry_run)
    return ok


def clean_jdtls_workspace(workspace: pathlib.Path, cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the given jdtls workspace if it is stale.

    Args:
        workspace: a jdtls workspace, such as
            ~/.cache/jdtls/jdtls-0a422631bbbc5fff4218961dc4439516f8308e91.
        cutoff: a time, in seconds since the epoch; a workspace in which nothing has
            changed since the cutoff is stale.
        dry_run: true if the workspace should be reported, but not deleted.

    Returns:
        False if the deletion failed.
    """
    covered = jdtls_covered_paths(workspace)
    # If the workspace records no covered paths, its age alone decides.
    orphaned = bool(covered) and not any(exists(path) for path in covered)
    if orphaned or not changed_since(workspace, cutoff):
        return delete(workspace, dry_run)
    return True


def main() -> None:
    """Delete, or with --dry-run report, the stale directories."""
    parser = argparse.ArgumentParser(
        description="Delete stale Claude Code temporary directories and jdtls workspaces."
    )
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="report the directories that would be deleted, but do not delete them",
    )
    parser.add_argument(
        "--days",
        type=float,
        default=7,
        metavar="DAYS",
        help="delete scratchpads and jdtls workspaces in which nothing has changed for"
        " this many days (default: %(default)s)",
    )
    parser.add_argument(
        "tmpdirs",
        nargs="*",
        type=pathlib.Path,
        metavar="TMPDIR",
        help="a directory that contains Claude Code project subdirectories or jdtls"
        " workspaces (default: each of /tmp/claude-$UID, $CLAUDE_CODE_TMPDIR/claude-$UID,"
        " and ~/.cache/jdtls (on macOS, ~/Library/Caches/jdtls) that exists)",
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
        candidates = [base / f"claude-{os.getuid()}" for base in sorted(bases)] + [JDTLS_CACHE]
        tmpdirs = [tmpdir for tmpdir in candidates if tmpdir.is_dir()]

    cutoff = time.time() - args.days * 24 * 60 * 60
    ok = True
    for tmpdir in tmpdirs:
        for child in sorted(tmpdir.iterdir()):
            if child.is_symlink() or not child.is_dir():
                continue
            if child.name.startswith("-"):
                ok &= clean_claude_project(child, cutoff, args.dry_run)
            elif child.name.startswith("jdtls-"):
                ok &= clean_jdtls_workspace(child, cutoff, args.dry_run)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
