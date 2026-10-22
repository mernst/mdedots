#!/usr/bin/env python3
"""Cancel the unfinished GitHub Actions runs of blocked PRs.

Usage (run in the directory that holds the PR status documents, as for pr-status.py):
  cancel-blocked-ci.py             # cancel the runs
  cancel-blocked-ci.py --dry-run   # report the runs that would be cancelled (alias: -n)

A PR is blocked if it depends on another open PR, as pr-status.py computes it
from pr-status-config.json.  Excluded PRs are never blocked.
"""

import importlib.util
import json
import pathlib
import subprocess
import sys

spec = importlib.util.spec_from_file_location(
    "pr_status", pathlib.Path(__file__).resolve().parent / "pr-status.py"
)
assert spec and spec.loader
pr_status = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pr_status)


UNFINISHED = ["queued", "in_progress", "waiting", "requested", "pending"]


def unfinished_runs(repo):
    """Find the runs in repo that have not completed.

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
                    f"repos/{repo.gh}/actions/runs?status={status}&per_page=100",
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
    with pathlib.Path(pr_status.CONFIG).open() as f:
        cfg = json.load(f)
    for name in cfg["order"]:
        repo = pr_status.Repo(cfg["repos"][name])
        pr_status.load_repo(repo, pairwise=False)
        runs = unfinished_runs(repo)
        for n in pr_status.Model(repo, cfg).blocked():
            pr = repo.prs[n]
            for r in runs.get((pr["owner"], pr["headRefName"]), []):
                print(
                    f"{repo.label} {repo.ref(n)}: {'would cancel' if dry_run else 'cancelling'}"
                    f" {r['status']} run {r['id']} ({r['name']}) {r['html_url']}"
                )
                if not dry_run:
                    result = subprocess.run(
                        ["gh", "run", "cancel", "-R", repo.gh, str(r["id"])],
                        check=False,
                        capture_output=True,
                        text=True,
                    )
                    if result.returncode != 0:
                        print(f"  failed: {result.stderr.strip()}", file=sys.stderr)


if __name__ == "__main__":
    main()
