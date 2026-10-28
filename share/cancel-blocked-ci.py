#!/usr/bin/env python3
"""Cancel the unfinished CI runs of blocked PRs.

Usage (run in the directory that holds the PR status documents, as for pr-status.py):
  cancel-blocked-ci.py             # cancel the runs
  cancel-blocked-ci.py --dry-run   # report the runs that would be cancelled (alias: -n)

A PR is blocked if it depends on another open PR, as pr-status.py computes it
from pr-status-config.json.  Excluded PRs are never blocked.  If there is no
pr-status-config.json, the blocked PRs are those in the "Blocked PRs" tables of
pr-dependencies.md, as pr-status.py most recently generated it.

The CI systems are GitHub Actions, Azure Pipelines, and CircleCI.  For Azure
Pipelines and CircleCI, only the builds of each PR's head commit are cancelled.
Credentials:
  Azure Pipelines: a personal access token in $AZURE_DEVOPS_EXT_PAT, or else
    the account that `az login` logged in.
  CircleCI: a personal API token in $CIRCLECI_CLI_TOKEN, or else the one in
    ~/.circleci/cli.yml or (for the snap package) ~/snap/circleci/current/.circleci/cli.yml,
    as written by `circleci setup`.
"""

import base64
import functools
import importlib.util
import json
import os
import pathlib
import re
import subprocess
import sys
import urllib.error
import urllib.request

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

# The URL of an Azure Pipelines build, such as
# "https://dev.azure.com/checkerframework/4dcb7892-.../_build/results?buildId=24267".
AZURE_BUILD = re.compile(
    r"https://(?:dev\.azure\.com/([^/]+)|([^/.]+)\.visualstudio\.com)/([^/]+)/_build/results\?buildId=(\d+)"
)
# The URL of a CircleCI workflow, such as "https://app.circleci.com/workflow/ecc46ce8-...".
CIRCLECI_WORKFLOW = re.compile(r"circleci\.com/.*\bworkflows?(?:-run)?/([0-9a-f-]{36})")
# The URL of a CircleCI job, such as "https://circleci.com/gh/circleci/circleci-docs/230602".
CIRCLECI_JOB = re.compile(r"circleci\.com/gh/([^/]+/[^/?]+)/(\d+)")
# The Azure DevOps resource, for which `az` issues access tokens.
AZURE_DEVOPS_RESOURCE = "499b84ac-1321-427f-aa17-267ca6975798"
# The configuration files of the CircleCI CLI, which `circleci setup` writes.
CIRCLECI_CONFIGS = ["~/.circleci/cli.yml", "~/snap/circleci/current/.circleci/cli.yml"]


def pr_name(label, ref):
    """Name a PR in output.

    Returns:
        ref, preceded by the repository's label unless ref already starts with it.
    """
    return ref if ref.startswith(label) else f"{label} {ref}"


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
                        "number,headRefName,headRefOid,headRepositoryOwner",
                    ]
                )
            )
        }
        blocked = []
        for ref, n in prs:
            if n not in open_prs:
                print(f"{pr_name(label, ref)}: not open; skipping it", file=sys.stderr)
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


def request(method, url, headers, body=None):
    """Make an HTTP request.

    Returns:
        The response body, parsed as JSON, or None if it is empty.
    """
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(
        url, data=data, method=method, headers={"Content-Type": "application/json", **headers}
    )
    with urllib.request.urlopen(req) as response:
        text = response.read()
    return json.loads(text) if text else None


@functools.cache
def azure_headers():
    """Determine how to authenticate to Azure DevOps.

    Returns:
        The HTTP headers that authenticate to Azure DevOps.
    """
    pat = os.environ.get("AZURE_DEVOPS_EXT_PAT")
    if pat:
        return {"Authorization": "Basic " + base64.b64encode(f":{pat}".encode()).decode()}
    token = pr_status.run(
        [
            "az",
            "account",
            "get-access-token",
            "--resource",
            AZURE_DEVOPS_RESOURCE,
            "--query",
            "accessToken",
            "-o",
            "tsv",
        ]
    ).strip()
    return {"Authorization": f"Bearer {token}"}


@functools.cache
def circleci_headers():
    """Determine how to authenticate to CircleCI.

    If there is no token, warns that CircleCI will refuse to cancel workflows.

    Returns:
        The HTTP headers that authenticate to CircleCI, or no headers if there is no token.
    """
    token = os.environ.get("CIRCLECI_CLI_TOKEN")
    for cli_yml in CIRCLECI_CONFIGS:
        if token:
            break
        cli_yml = pathlib.Path(cli_yml).expanduser()
        if cli_yml.exists():
            match = re.search(r"^token:\s*(\S+)", cli_yml.read_text(), re.MULTILINE)
            token = match and match[1]
    if not token:
        print(
            "cancel-blocked-ci.py: no CircleCI token found in $CIRCLECI_CLI_TOKEN or in "
            + " or ".join(CIRCLECI_CONFIGS)
            + ", so CircleCI will refuse to cancel workflows;"
            " create a personal API token and run `circleci setup`",
            file=sys.stderr,
        )
    return {"Circle-Token": token} if token else {}


def circleci_workflow(url):
    """Find the CircleCI workflow that url shows, which is a workflow's or a job's URL.

    Returns:
        The id of the workflow, or None if there is none.
    """
    if match := CIRCLECI_WORKFLOW.search(url):
        return match[1]
    if match := CIRCLECI_JOB.search(url):
        try:
            job = request(
                "GET",
                f"https://circleci.com/api/v2/project/gh/{match[1]}/job/{match[2]}",
                circleci_headers(),
            )
        except urllib.error.URLError as e:
            print(f"  failed to look up {url}: {failure(e)}", file=sys.stderr)
            return None
        return ((job or {}).get("latest_workflow") or {}).get("id")
    return None


def unfinished_other_builds(gh, sha):
    """Find the Azure Pipelines and CircleCI builds of commit sha that have not completed.

    Returns:
        A list of the status, description, URL, and cancelling function of each build.
        The cancelling function takes no arguments.
    """
    # The status, name, and URL of each unfinished check run and commit status.
    unfinished = []
    for page in json.loads(
        pr_status.run(
            [
                "gh",
                "api",
                "--paginate",
                "--slurp",
                f"repos/{gh}/commits/{sha}/check-runs?per_page=100",
            ]
        )
    ):
        unfinished.extend(
            (c["status"], c["name"], c["details_url"])
            for c in page["check_runs"]
            if c["status"] != "completed" and c["details_url"]
        )
    status = json.loads(pr_status.run(["gh", "api", f"repos/{gh}/commits/{sha}/status"]))
    unfinished.extend(
        ("pending", s["context"], s["target_url"])
        for s in status["statuses"]
        if s["state"] == "pending" and s["target_url"]
    )

    builds = {}  # identifier -> (status, description, URL, cancelling function)
    for state, name, url in unfinished:
        if match := AZURE_BUILD.search(url):
            org, project, build = match[1] or match[2], match[3], match[4]
            api = (
                f"https://dev.azure.com/{org}/{project}/_apis/build/builds/{build}?api-version=7.1"
            )
            builds.setdefault(
                ("azure", org, project, build),
                (
                    state,
                    f"Azure Pipelines build {build} ({name})",
                    match[0],
                    lambda api=api: request(
                        "PATCH", api, azure_headers(), {"status": "cancelling"}
                    ),
                ),
            )
        elif "circleci.com" in url:
            workflow = circleci_workflow(url)
            if workflow is None:
                print(f"  no CircleCI workflow found for {url}", file=sys.stderr)
                continue
            api = f"https://circleci.com/api/v2/workflow/{workflow}/cancel"
            builds.setdefault(
                ("circleci", workflow),
                (
                    state,
                    f"CircleCI workflow {workflow} ({name})",
                    f"https://app.circleci.com/workflow/{workflow}",
                    lambda api=api: request("POST", api, circleci_headers()),
                ),
            )
    return list(builds.values())


def failure(e):
    """Describe a failed command or HTTP request.

    Returns:
        A description of e, including any error output or error response.
    """
    if isinstance(e, subprocess.CalledProcessError):
        return e.stderr.strip()
    if isinstance(e, urllib.error.HTTPError):
        return f"{e} {error_message(e.read().decode(errors='replace'))}".strip()
    return str(e)


def error_message(text):
    """Extract the explanation from the body of an HTTP error response.

    The body may be a long HTML page.

    Returns:
        One line:  the "message" of a JSON body, the title of an HTML body, or else the
        start of the body.
    """
    try:
        body = json.loads(text)
        if isinstance(body, dict) and isinstance(body.get("message"), str):
            text = body["message"]
    except ValueError:
        if match := re.search(r"<title>(.*?)</title>", text, re.DOTALL | re.IGNORECASE):
            text = match[1]
    text = " ".join(text.split())
    return text if len(text) <= 200 else text[:200] + "..."


def cancel(cancelling):
    """Call the cancelling function, reporting any failure."""
    try:
        cancelling()
    except (urllib.error.URLError, subprocess.CalledProcessError) as e:
        print(f"  failed: {failure(e)}", file=sys.stderr)


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
            builds = [
                (
                    r["status"],
                    f"run {r['id']} ({r['name']})",
                    r["html_url"],
                    lambda r=r, gh=gh: pr_status.run(
                        ["gh", "run", "cancel", "-R", gh, str(r["id"])]
                    ),
                )
                for r in runs.get((pr["owner"], pr["headRefName"]), [])
            ]
            builds += unfinished_other_builds(gh, pr["headRefOid"])
            for status, description, url, cancelling in builds:
                print(
                    f"{pr_name(label, ref)}: {'would cancel' if dry_run else 'cancelling'}"
                    f" {status} {description} {url}"
                )
                if not dry_run:
                    cancel(cancelling)


if __name__ == "__main__":
    main()
