#!/usr/bin/env python3
"""Cancel the unfinished GitHub Actions runs of blocked PRs.

Usage (run in the directory that holds the PR status documents, as for pr-status.py):
  cancel-blocked-ci.py             # cancel the runs
  cancel-blocked-ci.py --dry-run   # report the runs that would be cancelled (alias: -n)

A PR is blocked if it depends on another open PR, as pr-status.py computes it
from pr-status-config.json.  Excluded PRs are never blocked.  If there is no
pr-status-config.json, the blocked PRs are those in the "Blocked PRs" tables of
pr-dependencies.md, as pr-status.py most recently generated it.
"""

import importlib.util
import json
import pathlib
import re
import subprocess
import sys

spec = importlib.util.spec_from_file_location(
    "pr_status", pathlib.Path(__file__).resolve().parent / "pr-status.py"
)
assert spec and spec.loader
pr_status = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pr_status)


UNFINISHED = ["queued", "in_progress", "waiting", "requested", "pending"]

# The dependency document that pr-status.py writes when there is no config file.
DEPENDENCIES = "pr-dependencies.md"

BLOCKED_SECTION = re.compile(
    r"<!-- BEGIN GENERATED: (blocked-[\w-]+) -->(.*?)<!-- END GENERATED: \1 -->", re.DOTALL
)
# The first cell of a PR table row, such as
# "| [daikon#555](https://github.com/codespecs/daikon/pull/555) |".
PR_ROW = re.compile(
    r"^\| \[([^\]]+)\]\(https://github\.com/([^/]+/[^/]+)/pull/(\d+)\)", re.MULTILINE
)


def blocked_from_config():
    """Find the blocked PRs, as pr-status.py computes them from its config.

    Returns:
        For each repository, a tuple of its label, its GitHub name, and a list of the
        reference and gh JSON of each of its blocked PRs.
    """
    with pathlib.Path(pr_status.CONFIG).open() as f:
        cfg = json.load(f)
    return [
        (m.repo.label, m.repo.gh, [(m.repo.ref(n), m.repo.prs[n]) for n in m.blocked()])
        for m in pr_status.load_models(cfg, pairwise=False)
    ]


def blocked_from_document():
    """Find the blocked PRs that the dependency document lists.

    Returns:
        For each repository, a tuple of its label, its GitHub name, and a list of the
        reference and gh JSON of each of its blocked PRs that is still open.
    """
    path = pathlib.Path(DEPENDENCIES)
    if not path.exists():
        sys.exit(f"cancel-blocked-ci.py: neither {pr_status.CONFIG} nor {DEPENDENCIES} exists")
    refs = {}  # GitHub name -> list of (reference, number)
    for section in BLOCKED_SECTION.finditer(path.read_text()):
        for row in PR_ROW.finditer(section[2]):
            refs.setdefault(row[2], []).append((row[1], int(row[3])))
    result = []
    for gh, prs in refs.items():
        # The label that pr-status.py's default config gives the repository.
        label = gh.split("/")[1]
        open_prs = {
            pr["number"]: pr
            for pr in json.loads(
                pr_status.run(
                    [
                        "gh",
                        "pr",
                        "list",
                        "-R",
                        gh,
                        "--state",
                        "open",
                        "--limit",
                        "300",
                        "--json",
                        "number,headRefName,headRepositoryOwner",
                    ]
                )
            )
        }
        blocked = []
        for ref, n in prs:
            if n not in open_prs:
                print(f"{label} {ref}: not open; skipping it", file=sys.stderr)
                continue
            pr = open_prs[n]
            pr["owner"] = (pr.get("headRepositoryOwner") or {}).get("login", "")
            blocked.append((ref, pr))
        result.append((label, gh, blocked))
    return result


def unfinished_runs(gh):
    """Find the runs in the GitHub repository gh that have not completed.

    Returns:
        A map from (head repository owner, branch) to the unfinished runs of that branch.
    """
    runs = {}
    for status in UNFINISHED:
        pages = json.loads(
            pr_status.run(
                [
                    "gh",
                    "api",
                    "--paginate",
                    "--slurp",
                    f"repos/{gh}/actions/runs?status={status}&per_page=100",
                ]
            )
        )
        for page in pages:
            for r in page["workflow_runs"]:
                owner = (
                    (r["head_repository"] or {}).get("owner", {}).get("login")
                )  # None if the fork is gone
                runs.setdefault((owner, r["head_branch"]), []).append(r)
    return runs


def main():
    """Cancel, or with --dry-run report, the unfinished runs of blocked PRs."""
    dry_run = bool({"--dry-run", "-n"} & set(sys.argv[1:]))
    if pathlib.Path(pr_status.CONFIG).exists():
        repos = blocked_from_config()
    else:
        repos = blocked_from_document()
    for label, gh, blocked in repos:
        if not blocked:
            continue
        runs = unfinished_runs(gh)
        for ref, pr in blocked:
            for r in runs.get((pr["owner"], pr["headRefName"]), []):
                print(
                    f"{label} {ref}: {'would cancel' if dry_run else 'cancelling'}"
                    f" {r['status']} run {r['id']} ({r['name']}) {r['html_url']}"
                )
                if not dry_run:
                    result = subprocess.run(
                        ["gh", "run", "cancel", "-R", gh, str(r["id"])],
                        check=False,
                        capture_output=True,
                        text=True,
                    )
                    if result.returncode != 0:
                        print(f"  failed: {result.stderr.strip()}", file=sys.stderr)


if __name__ == "__main__":
    main()
