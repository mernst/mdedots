#!/usr/bin/env python3
"""Recover disk space by deleting stale data that tools regenerate on demand.

Usage:
  clean-disk-space.py [--dry-run] [--days DAYS] [--jdtls-days DAYS]
                      [--unused-days DAYS] [--no-sudo] [TMPDIR ...]

If TMPDIR arguments are given, the script cleans only those directories, as
described below.  Otherwise, it performs these steps, each of which is skipped
if the directory or command that it needs does not exist:
 * It cleans each of /tmp/claude-$UID, $CLAUDE_CODE_TMPDIR/claude-$UID, and
   ~/.cache/jdtls (on macOS, ~/Library/Caches/jdtls) as a TMPDIR.
 * If no GNOME file indexer (localsearch or tracker-miner-fs) is running, it
   deletes the file indexer's database ~/.cache/tracker3.  The indexer rebuilds
   it when it next runs.
 * It runs `uv cache prune`, `prek cache gc`, and `npm cache clean --force`.
 * In the pip and gh caches, it deletes each file that has not been read for
   UNUSED-DAYS days (default 30).
 * Under ~/research and ~/java, it deletes the build output of each git working
   copy directory:  each directory named "build" or "target", at most 3 levels
   below the working copy directory's top, that git ignores, that contains no
   tracked files, and in which nothing has changed for UNUSED-DAYS days.  The
   next build regenerates it.  A git repository nested in a working copy
   directory, such as a clone, is a working copy directory of its own:  the
   nested repository, not the enclosing one, decides whether a directory in it
   is build output.
 * Unless --no-sudo is given, it runs these commands via sudo:  it shrinks the
   systemd journal to 500 MB, removes disabled snap revisions and makes snapd
   retain only 2 revisions of each snap, and deletes downloaded apt packages and
   the packages that `apt-get autoremove` removes, such as old kernels.  sudo
   can prompt for a password.  If sudo needs a password and standard input is
   not a terminal, the script skips these commands and exits with status 1.

The script cleans two kinds of subdirectory of a TMPDIR:
 * A Claude Code project subdirectory, whose name is the absolute path of a
   project directory with every non-alphanumeric character replaced by "-".
   For example, /home/mernst/.emacs.d becomes -home-mernst--emacs-d.  A project
   subdirectory corresponds to its project directory.  It contains one
   subdirectory per session, which can contain a scratchpad directory.
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
It never deletes a jdtls workspace that a running process uses, that is, one
that a running process's command line names after "-data".
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
import collections
import contextlib
import functools
import itertools
import os
import pathlib
import platform
import re
import shlex
import shutil
import stat
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

# The XDG base directory for per-user caches, which gh and GNOME's file indexer use
# on every platform.
XDG_CACHE_HOME = pathlib.Path(os.environ.get("XDG_CACHE_HOME") or pathlib.Path.home() / ".cache")

# The platform's directory for per-user caches, which pip uses.
PLATFORM_CACHE_HOME = (
    pathlib.Path.home() / "Library" / "Caches" if platform.system() == "Darwin" else XDG_CACHE_HOME
)

# The names of the programs of GNOME's file indexer:  localsearch-3, and
# tracker-miner-fs-3 through GNOME 46.  Both keep their database in
# XDG_CACHE_HOME/tracker3.
FILE_INDEXER_PROGRAMS = {"localsearch-3", "tracker-miner-fs-3"}

# The directories under which this script deletes the build output of git working
# copy directories.
BUILD_ROOTS = [pathlib.Path.home() / "research", pathlib.Path.home() / "java"]

# How deep below a BUILD_ROOTS directory this script looks for working copy
# directories.  A working copy directory that is nested in another one can be
# deeper.
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


def size_if_unchanged_since(directory: pathlib.Path, cutoff: float) -> int | None:
    """Return the total size of the files under the given directory, if none has changed.

    A file's change time is the later of its modification time and its status
    change time, so that a file extracted from an archive counts as changed when
    it was extracted.

    Args:
        directory: a directory.
        cutoff: a time, in seconds since the epoch.

    Returns:
        The total size, in bytes, of the files under `directory`, or None if the
        directory or any file or directory under it changed after `cutoff`.
    """
    total = 0
    for dirpath, dirnames, filenames in directory.walk():
        for name in [".", *dirnames, *filenames]:
            try:
                st = (dirpath / name).lstat()
            except OSError:
                continue
            if max(st.st_mtime, st.st_ctime) > cutoff:
                return None
            if not stat.S_ISDIR(st.st_mode):
                total += st.st_size
    return total


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


def delete(path: pathlib.Path, dry_run: bool, size: int | None = None) -> bool:
    """Delete, or with `dry_run` report, the given directory.

    Args:
        path: a directory.
        dry_run: true if the directory should be reported, but not deleted.
        size: the total size, in bytes, of the files under `path`, or None to
            compute it; used for diagnostics.

    Returns:
        False if the deletion failed.
    """
    size_text = format_size(size_of(path) if size is None else size)
    if dry_run:
        print(f"Would delete {path} ({size_text})")
        return True
    print(f"Deleting {path} ({size_text})")
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
        if scratchpad.is_dir() and not scratchpad.is_symlink():
            size = size_if_unchanged_since(scratchpad, cutoff)
            if size is not None:
                ok &= delete(scratchpad, dry_run, size)
    return ok


@functools.cache
def process_command_lines() -> list[list[str]] | None:
    """Return the command lines of the running processes.

    A command line's arguments are split at whitespace, so an argument that
    contains whitespace becomes several elements.

    Returns:
        Each running process's program and arguments, such as
        ["/usr/bin/java", "-jar", "launcher.jar", "-data", "/home/me/ws"], or None
        if `ps` does not exist or fails.
    """
    try:
        ps = subprocess.run(
            ["ps", "-A", "-ww", "-o", "args="], capture_output=True, text=True, check=False
        )
    except OSError:
        return None
    if ps.returncode != 0:
        return None
    return [line.split() for line in ps.stdout.splitlines()]


def jdtls_workspace_in_use(workspace: pathlib.Path) -> bool:
    """Return true if a running process uses the given jdtls workspace.

    jdtls neither locks nor holds open any file in its workspace, so this checks
    whether a running process's command line names the workspace after "-data".

    Args:
        workspace: a jdtls workspace, such as
            ~/.cache/jdtls/jdtls-0a422631bbbc5fff4218961dc4439516f8308e91.

    Returns:
        True if a running process uses `workspace`, or if the running processes
        cannot be listed.
    """
    command_lines = process_command_lines()
    if command_lines is None:
        return True
    target = workspace.resolve()
    return any(
        arg == "-data" and pathlib.Path(value).resolve() == target
        for args in command_lines
        for arg, value in itertools.pairwise(args)
    )


def clean_jdtls_workspace(workspace: pathlib.Path, cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the given jdtls workspace if it is stale and unused.

    Args:
        workspace: a jdtls workspace, such as
            ~/.cache/jdtls/jdtls-0a422631bbbc5fff4218961dc4439516f8308e91.
        cutoff: a time, in seconds since the epoch; a workspace in which nothing has
            changed since the cutoff is stale.
        dry_run: true if the workspace should be reported, but not deleted.

    Returns:
        False if the deletion failed.
    """
    if jdtls_workspace_in_use(workspace):
        return True
    covered = jdtls_covered_paths(workspace)
    # If the workspace records no covered paths, its age alone decides.
    if covered and not any(exists(path) for path in covered):
        return delete(workspace, dry_run)
    size = size_if_unchanged_since(workspace, cutoff)
    if size is not None:
        return delete(workspace, dry_run, size)
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


def run(command: list[str], dry_run: bool, omit: re.Pattern[str] | None = None) -> bool:
    """Run, or with `dry_run` print, the given command, if its program exists.

    Args:
        command: a command and its arguments, such as ["uv", "cache", "prune"].  If
            command[0] is "sudo", command[1] is the program that must exist.
        dry_run: true if the command should be printed, but not run.
        omit: matches the lines of the command's output that should not be printed;
            if every line matches, the command itself is not printed either.  None
            means to print the command, and then its output as it is produced.

    Returns:
        False if the command failed.
    """
    program = command[1] if command[0] == "sudo" else command[0]
    if shutil.which(program) is None:
        return True
    if dry_run or omit is None:
        print(f"+ {shlex.join(command)}", flush=True)
    if dry_run:
        return True
    try:
        if omit is None:
            status = subprocess.run(command, check=False).returncode
        else:
            # sudo writes its password prompt to the terminal, so capturing the output
            # does not hide the prompt.
            result = subprocess.run(command, capture_output=True, text=True, check=False)
            status = result.returncode
            kept = [
                (line, stream)
                for output, stream in ((result.stdout, sys.stdout), (result.stderr, sys.stderr))
                for line in output.splitlines()
                if not omit.search(line)
            ]
            if kept:
                print(f"+ {shlex.join(command)}", flush=True)
            for line, stream in kept:
                print(line, file=stream)
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
    if not unread:
        return True
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


def file_indexer_running() -> bool:
    """Return true if GNOME's file indexer is running.

    Returns:
        True if a running process's program is in FILE_INDEXER_PROGRAMS, or if the
        running processes cannot be listed.
    """
    command_lines = process_command_lines()
    if command_lines is None:
        return True
    return any(
        args and pathlib.Path(args[0]).name in FILE_INDEXER_PROGRAMS for args in command_lines
    )


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
    tracker3 = XDG_CACHE_HOME / "tracker3"
    if tracker3.is_dir() and not file_indexer_running():
        ok &= delete(tracker3, dry_run)
    ok &= run(["uv", "cache", "prune"], dry_run)
    ok &= run(["prek", "cache", "gc"], dry_run)
    ok &= run(["npm", "cache", "clean", "--force"], dry_run)
    ok &= delete_unread_files(PLATFORM_CACHE_HOME / "pip", unused_cutoff, dry_run)
    ok &= delete_unread_files(XDG_CACHE_HOME / "gh", unused_cutoff, dry_run)
    return ok


def build_output_candidates(root: pathlib.Path) -> dict[pathlib.Path, list[pathlib.Path]]:
    """Return the candidate build-output directories under the given directory.

    A working copy directory is a directory that contains a ".git" file or
    directory and that is at most MAX_WORKING_COPY_DEPTH levels below `root`
    (including `root` itself) or is nested in another working copy directory.
    A candidate of a working copy directory is a directory that the working copy
    directory is the innermost working copy directory to contain, whose name is
    in BUILD_DIR_NAMES, that is at most MAX_BUILD_DIR_DEPTH levels below the
    working copy directory, that is not itself a working copy directory, and
    that is under no other candidate.

    Args:
        root: a directory, such as ~/research.

    Returns:
        A dict that maps each working copy directory that has candidates, such as
        ~/research/proj, to its candidates, such as [~/research/proj/build].
    """
    result = collections.defaultdict(list)
    # Maps each directory that the walk has yet to visit to the innermost working
    # copy directory that contains it, or to None if none does.
    owners: dict[pathlib.Path, pathlib.Path | None] = {root: None}
    for dirpath, dirnames, filenames in root.walk():
        owner = owners.pop(dirpath)
        if ".git" in dirnames or ".git" in filenames:
            owner = dirpath
        # True if the walk visits subdirectories to look for working copy directories.
        visit_for_working_copies = len(dirpath.relative_to(root).parts) < MAX_WORKING_COPY_DEPTH
        # How deep below the owner the subdirectories are.
        subdir_depth = None if owner is None else len(dirpath.relative_to(owner).parts) + 1
        visited = []
        for name in dirnames:
            if name in UNSEARCHED_DIR_NAMES:
                continue
            subdir = dirpath / name
            if name in BUILD_DIR_NAMES and not (subdir / ".git").exists():
                if subdir_depth is not None and subdir_depth <= MAX_BUILD_DIR_DEPTH:
                    result[owner].append(subdir)
                continue
            if visit_for_working_copies or (
                subdir_depth is not None and subdir_depth < MAX_BUILD_DIR_DEPTH
            ):
                visited.append(name)
                owners[subdir] = owner
        dirnames[:] = visited
    return result


def ignored_and_untracked(
    working_copy: pathlib.Path, directories: list[pathlib.Path]
) -> list[pathlib.Path]:
    """Return the given directories that git ignores and under which it tracks no file.

    Args:
        working_copy: a git working copy directory.
        directories: directories in the working copy directory.

    Returns:
        Each element of `directories` that git ignores and under which it tracks
        no file.  Returns no directories if git fails.
    """
    git = ["git", "-C", str(working_copy)]
    relative = [str(d.relative_to(working_copy)) for d in directories]
    try:
        check_ignore = subprocess.run(
            [*git, "check-ignore", "--stdin", "-z"],
            input="".join(f"{r}\0" for r in relative),
            capture_output=True,
            text=True,
            check=False,
        )
        # Exit status 1 means that git ignores none of the paths.
        if check_ignore.returncode != 0:
            return []
        ignored = [r for r in check_ignore.stdout.split("\0") if r]
        ls_files = subprocess.run(
            [*git, "--literal-pathspecs", "ls-files", "-z", "--", *ignored],
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return []
    if ls_files.returncode != 0:
        return []
    # Each directory that contains a tracked file.
    tracked_dirs = {
        str(parent)
        for tracked in ls_files.stdout.split("\0")
        if tracked
        for parent in pathlib.PurePath(tracked).parents
    }
    return [working_copy / r for r in ignored if r not in tracked_dirs]


def clean_build_output(unused_cutoff: float, dry_run: bool) -> bool:
    """Delete, or with `dry_run` report, the stale build output under BUILD_ROOTS.

    Args:
        unused_cutoff: a time, in seconds since the epoch; build output in which
            nothing has changed since the cutoff is stale.
        dry_run: true if the directories should be reported, but not deleted.

    Returns:
        False if a deletion failed.
    """
    if shutil.which("git") is None:
        return True
    ok = True
    for root in BUILD_ROOTS:
        if not root.is_dir():
            continue
        for working_copy, candidates in build_output_candidates(root).items():
            for build_dir in ignored_and_untracked(working_copy, candidates):
                size = size_if_unchanged_since(build_dir, unused_cutoff)
                if size is not None:
                    ok &= delete(build_dir, dry_run, size)
    return ok


def clean_system(dry_run: bool) -> bool:
    """Run, or with `dry_run` print, the commands that delete system-wide data via sudo.

    Args:
        dry_run: true if the commands should be printed, but not run.

    Returns:
        False if a command failed, or if sudo needs a password and standard input
        is not a terminal.
    """
    if (
        not dry_run
        and shutil.which("sudo") is not None
        and not sys.stdin.isatty()
        and subprocess.run(["sudo", "-n", "true"], capture_output=True, check=False).returncode != 0
    ):
        print(
            "Skipping the sudo commands:  sudo needs a password, and standard input is not"
            " a terminal.  Pass --no-sudo to skip them silently.",
            file=sys.stderr,
        )
        return False
    ok = run(
        ["sudo", "journalctl", "--vacuum-size=500M"],
        dry_run,
        omit=re.compile(r"^Vacuuming done, freed 0B "),
    )
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
    ok &= run(["sudo", "apt-get", "autoremove", "--purge", "--yes"], dry_run)
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
        " workspaces; if any is given, clean only these directories",
    )
    args = parser.parse_args()

    only_tmpdirs = bool(args.tmpdirs)
    if only_tmpdirs:
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
    if not only_tmpdirs:
        ok &= clean_tool_caches(unused_cutoff, args.dry_run)
        ok &= clean_build_output(unused_cutoff, args.dry_run)
        if not args.no_sudo:
            ok &= clean_system(args.dry_run)
    print_free_space()
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
