#!/usr/bin/env python3
"""Recover disk space by deleting stale data that tools regenerate on demand.

Usage:
  clean-disk-space.py [--dry-run] [--days DAYS] [--jdtls-days DAYS]
                      [--unused-days DAYS] [--no-sudo] [TMPDIR ...]

The script performs these steps, each of which is skipped if the directory or
command that it needs does not exist:
 * It cleans each TMPDIR, as described below.
 * It deletes the stale GNOME file-indexer database ~/.cache/tracker3.
 * It runs `uv cache prune`, `prek cache gc`, and `npm cache clean --force`.
 * In the pip and gh caches, it deletes each file that has not been read for
   UNUSED-DAYS days (default 30).
 * Under ~/research and ~/java, it deletes the build output of each git working
   copy directory:  each directory named "build" or "target", at most 3 levels
   below the working copy directory's top, that git ignores, that contains no
   tracked files, and in which nothing has changed for UNUSED-DAYS days.  The
   next build regenerates it.
 * Unless --no-sudo is given, it runs these commands via sudo, which can prompt
   for a password:  it shrinks the systemd journal to 500 MB, removes disabled
   snap revisions and makes snapd retain only 2 revisions of each snap, and
   deletes downloaded apt packages and the packages that `apt-get autoremove`
   removes, such as old kernels.

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

In a TMPDIR, this script deletes:
 * each project subdirectory and each jdtls workspace that corresponds to no
   existing file or directory, and
 * each session's scratchpad directory in which nothing has changed for DAYS
   days (default 7), and
 * each jdtls workspace in which nothing has changed for JDTLS-DAYS days
   (default 2).  A deleted workspace is rebuilt when an editor next opens one of
   its projects.
It does not touch the TMPDIR's other files or directories.

Because the Claude Code encoding is lossy, the script decodes a project
subdirectory's name by walking the file system from "/":  at each directory, it
descends into every subdirectory whose encoded name matches the next part of the
name.

Options:
  --dry-run, -n           report what would be deleted and the commands that
                          would be run, but do not delete or run anything
  --days DAYS             delete scratchpads in which nothing has changed for
                          this many days
  --jdtls-days DAYS       delete jdtls workspaces in which nothing has changed
                          for this many days
  --unused-days DAYS      delete cache files that have not been read, and build
                          output in which nothing has changed, for this many days
  --no-sudo               skip the steps that run commands via sudo
"""

import argparse
import contextlib
import functools
import os
import pathlib
import platform
import re
import shlex
import shutil
import subprocess
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

# The directory that holds per-user caches, such as those of pip and gh.
CACHE_HOME = (
    pathlib.Path.home() / "Library" / "Caches"
    if platform.system() == "Darwin"
    else pathlib.Path(os.environ.get("XDG_CACHE_HOME") or pathlib.Path.home() / ".cache")
)

# The directories under which this script deletes the build output of git working
# copy directories.
BUILD_ROOTS = [pathlib.Path.home() / "research", pathlib.Path.home() / "java"]

# How deep below a BUILD_ROOTS directory a working copy directory can be.
MAX_WORKING_COPY_DEPTH = 4

# The names of build-output directories.
BUILD_DIR_NAMES = {"build", "target"}

# How deep below a working copy directory's top a build-output directory can be.
MAX_BUILD_DIR_DEPTH = 3

# The names of directories in which this script looks for neither working copy
# directories nor build output.
UNSEARCHED_DIR_NAMES = {".git", "node_modules", ".venv"}


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


def size_of(directory: pathlib.Path) -> int:
    """Return the total size of the files under the given directory.

    Returns:
        The total size, in bytes, of the files under `directory`.
    """
    total = 0
    for dirpath, _, filenames in directory.walk():
        for name in filenames:
            try:
                total += (dirpath / name).lstat().st_size
            except OSError:
                continue
    return total


def format_size(size: int) -> str:
    """Return the given number of bytes as a human-readable string.

    Returns:
        `size` in gigabytes or megabytes, such as "1.5 GB".
    """
    if size >= 1e9:
        return f"{size / 1e9:.1f} GB"
    return f"{size / 1e6:.1f} MB"


def delete(path: pathlib.Path, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, a directory.

    Returns:
        False if the deletion failed.
    """
    size = format_size(size_of(path))
    if dry_run:
        print(f"Would delete {path} ({size})")
        return True
    print(f"Deleting {path} ({size})")
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


def clean_tmpdir(tmpdir: pathlib.Path, cutoff: float, jdtls_cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the stale subdirectories of the given TMPDIR.

    Args:
        tmpdir: a directory that contains Claude Code project subdirectories or jdtls
            workspaces, such as /tmp/claude-1000 or ~/.cache/jdtls.
        cutoff: a time, in seconds since the epoch; a scratchpad in which nothing has
            changed since the cutoff is stale.
        jdtls_cutoff: a time, in seconds since the epoch; a jdtls workspace in which
            nothing has changed since the cutoff is stale.
        dry_run: true if the directories should be reported, but not deleted.

    Returns:
        False if a deletion failed.
    """
    ok = True
    for child in sorted(tmpdir.iterdir()):
        if child.is_symlink() or not child.is_dir():
            continue
        if child.name.startswith("-"):
            ok &= clean_claude_project(child, cutoff, dry_run)
        elif child.name.startswith("jdtls-"):
            ok &= clean_jdtls_workspace(child, jdtls_cutoff, dry_run)
    return ok


def run(command: list[str], dry_run: bool) -> bool:
    """Run, or with `dry_run` print, the given command, if its program exists.

    Args:
        command: a command and its arguments, such as ["uv", "cache", "prune"].  If
            command[0] is "sudo", command[1] is the program that must exist.
        dry_run: true if the command should be printed, but not run.

    Returns:
        False if the command failed.
    """
    program = command[1] if command[0] == "sudo" else command[0]
    if shutil.which(program) is None:
        return True
    print(f"+ {shlex.join(command)}", flush=True)
    if dry_run:
        return True
    try:
        status = subprocess.run(command, check=False).returncode
    except OSError as e:
        print(f"Could not run {command[0]}: {e}", file=sys.stderr)
        return False
    if status != 0:
        print(f"{shlex.join(command)} failed with status {status}", file=sys.stderr)
        return False
    return True


def delete_unread_files(directory: pathlib.Path, cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the given directory's unread files.

    Also delete the subdirectories that this leaves empty.

    Args:
        directory: a cache directory, such as ~/.cache/pip.
        cutoff: a time, in seconds since the epoch; a file that has not been read
            since the cutoff is unread.
        dry_run: true if the files should be reported, but not deleted.

    Returns:
        False if a deletion failed.
    """
    if not directory.is_dir():
        return True
    unread = []
    size = 0
    for dirpath, _, filenames in directory.walk():
        for name in filenames:
            path = dirpath / name
            try:
                st = path.lstat()
            except OSError:
                continue
            if st.st_atime < cutoff:
                unread.append(path)
                size += st.st_size
    verb = "Would delete" if dry_run else "Deleting"
    print(f"{verb} {len(unread)} unread files ({format_size(size)}) under {directory}")
    if dry_run:
        return True
    ok = True
    for path in unread:
        try:
            path.unlink()
        except OSError as e:
            print(f"Could not delete {path}: {e}", file=sys.stderr)
            ok = False
    for dirpath, _, _ in directory.walk(top_down=False):
        if dirpath != directory:
            # rmdir fails if the directory is not empty.
            with contextlib.suppress(OSError):
                dirpath.rmdir()
    return ok


def clean_tool_caches(unused_cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the stale contents of tools' caches.

    Args:
        unused_cutoff: a time, in seconds since the epoch; a pip or gh cache file
            that has not been read since the cutoff is deleted.
        dry_run: true if the deletions should be reported, but not performed.

    Returns:
        False if a deletion or a command failed.
    """
    ok = True
    # GNOME's file indexer, localsearch, does not use this directory.
    tracker3 = CACHE_HOME / "tracker3"
    if tracker3.is_dir():
        ok &= delete(tracker3, dry_run)
    ok &= run(["uv", "cache", "prune"], dry_run)
    ok &= run(["prek", "cache", "gc"], dry_run)
    ok &= run(["npm", "cache", "clean", "--force"], dry_run)
    for name in ["pip", "gh"]:
        ok &= delete_unread_files(CACHE_HOME / name, unused_cutoff, dry_run)
    return ok


def working_copy_directories(root: pathlib.Path) -> list[pathlib.Path]:
    """Return the git working copy directories under the given directory.

    Args:
        root: a directory, such as ~/research.

    Returns:
        Each directory that contains a ".git" file or directory and is at most
        MAX_WORKING_COPY_DEPTH levels below `root`, including `root` itself.
    """
    result = []
    for dirpath, dirnames, filenames in root.walk():
        if ".git" in dirnames or ".git" in filenames:
            result.append(dirpath)
        if len(dirpath.relative_to(root).parts) >= MAX_WORKING_COPY_DEPTH:
            dirnames.clear()
        else:
            dirnames[:] = [d for d in dirnames if d not in UNSEARCHED_DIR_NAMES | BUILD_DIR_NAMES]
    return result


def build_output_directories(working_copy: pathlib.Path) -> list[pathlib.Path]:
    """Return the candidate build-output directories of the given working copy directory.

    Args:
        working_copy: a git working copy directory.

    Returns:
        Each directory whose name is in BUILD_DIR_NAMES and that is at most
        MAX_BUILD_DIR_DEPTH levels below `working_copy`, except one that is under
        another such directory.
    """
    result = []
    for dirpath, dirnames, _ in working_copy.walk():
        result.extend(dirpath / d for d in dirnames if d in BUILD_DIR_NAMES)
        if len(dirpath.relative_to(working_copy).parts) + 1 >= MAX_BUILD_DIR_DEPTH:
            dirnames.clear()
        else:
            dirnames[:] = [d for d in dirnames if d not in UNSEARCHED_DIR_NAMES | BUILD_DIR_NAMES]
    return result


def is_ignored_and_untracked(working_copy: pathlib.Path, path: pathlib.Path) -> bool:
    """Return true if git ignores the given path and tracks no file under it.

    Args:
        working_copy: a git working copy directory.
        path: a directory in the working copy directory.

    Returns:
        True if git ignores `path` and tracks no file under it.
    """
    relative = str(path.relative_to(working_copy))
    git = ["git", "-C", str(working_copy)]
    ignored = subprocess.run(
        [*git, "check-ignore", "-q", "--", relative], capture_output=True, check=False
    )
    if ignored.returncode != 0:
        return False
    tracked = subprocess.run(
        [*git, "ls-files", "--", relative], capture_output=True, text=True, check=False
    )
    return tracked.returncode == 0 and tracked.stdout == ""


def clean_build_output(unused_cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the stale build output under BUILD_ROOTS.

    Args:
        unused_cutoff: a time, in seconds since the epoch; build output in which
            nothing has changed since the cutoff is stale.
        dry_run: true if the directories should be reported, but not deleted.

    Returns:
        False if a deletion failed.
    """
    ok = True
    for root in BUILD_ROOTS:
        if not root.is_dir():
            continue
        for working_copy in working_copy_directories(root):
            for build_dir in build_output_directories(working_copy):
                if is_ignored_and_untracked(working_copy, build_dir) and not changed_since(
                    build_dir, unused_cutoff
                ):
                    ok &= delete(build_dir, dry_run)
    return ok


def clean_system(dry_run: bool) -> bool:
    """Run, or with `dry_run` print, the commands that delete system-wide data via sudo.

    Args:
        dry_run: true if the commands should be printed, but not run.

    Returns:
        False if a command failed.
    """
    ok = run(["sudo", "journalctl", "--vacuum-size=500M"], dry_run)
    if shutil.which("snap") is not None:
        ok &= run(["sudo", "snap", "set", "system", "refresh.retain=2"], dry_run)
        snaps = subprocess.run(
            ["snap", "list", "--all"], capture_output=True, text=True, check=False
        )
        for line in snaps.stdout.splitlines()[1:]:
            fields = line.split()
            if len(fields) >= 6 and "disabled" in fields[-1].split(","):
                ok &= run(["sudo", "snap", "remove", fields[0], f"--revision={fields[2]}"], dry_run)
    ok &= run(["sudo", "apt-get", "clean"], dry_run)
    ok &= run(["sudo", "apt-get", "autoremove", "--purge"], dry_run)
    return ok


def print_free_space() -> None:
    """Print the free space on the root file system."""
    print(f"Free space on /: {format_size(shutil.disk_usage('/').free)}", flush=True)


def main() -> None:
    """Delete, or with --dry-run report, the stale data."""
    parser = argparse.ArgumentParser(
        description="Recover disk space by deleting stale data that tools regenerate."
    )
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="report what would be deleted and the commands that would be run, but do"
        " not delete or run anything",
    )
    parser.add_argument(
        "--days",
        type=float,
        default=7,
        metavar="DAYS",
        help="delete scratchpads in which nothing has changed for this many days"
        " (default: %(default)s)",
    )
    parser.add_argument(
        "--jdtls-days",
        type=float,
        default=2,
        metavar="DAYS",
        help="delete jdtls workspaces in which nothing has changed for this many days"
        " (default: %(default)s)",
    )
    parser.add_argument(
        "--unused-days",
        type=float,
        default=30,
        metavar="DAYS",
        help="delete cache files that have not been read, and build output in which"
        " nothing has changed, for this many days (default: %(default)s)",
    )
    parser.add_argument(
        "--no-sudo",
        action="store_true",
        help="skip the steps that run commands via sudo",
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

    now = time.time()
    cutoff = now - args.days * 24 * 60 * 60
    jdtls_cutoff = now - args.jdtls_days * 24 * 60 * 60
    unused_cutoff = now - args.unused_days * 24 * 60 * 60
    print_free_space()
    ok = True
    for tmpdir in tmpdirs:
        ok &= clean_tmpdir(tmpdir, cutoff, jdtls_cutoff, args.dry_run)
    ok &= clean_tool_caches(unused_cutoff, args.dry_run)
    ok &= clean_build_output(unused_cutoff, args.dry_run)
    if not args.no_sudo:
        ok &= clean_system(args.dry_run)
    print_free_space()
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
