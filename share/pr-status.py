#!/usr/bin/env python3
"""Regenerate the PR status documents in the current directory.

Usage (run in the directory that holds the documents):
  pr-status.py            # snapshot, report changes, regenerate the documents
  pr-status.py --check    # snapshot and report changes, but write no document

Inputs:
  pr-status-config.json   hand-maintained:  repositories, notes, labels, decisions
  pr-status-snapshot.json written by the previous run; compared with this run
Each run:
  * reads the open PRs with gh,
  * runs pr-merge-analysis.sh (cached) on them, for each repository,
  * computes the dependencies:  a PR depends on another if its description says
    so, if its branch contains all of the other PR's non-merge commits, or if the
    config says so,
  * updates each modeled PR description's "Land after" and "Land before" lines,
  * prints what changed since the previous snapshot (input for the summary),
  * rewrites the unblocked documents entirely, and the parts of the dependency
    document between <!-- BEGIN GENERATED: name --> and <!-- END GENERATED: name -->.
"""

import datetime as dt
import hashlib
import itertools
import json
import operator
import pathlib
import re
import subprocess
import sys
from collections import defaultdict

CONFIG = "pr-status-config.json"
SNAPSHOT = "pr-status-snapshot.json"
ANALYSIS = str(pathlib.Path("~/bin/share/pr-merge-analysis.sh").expanduser())


def run(cmd):
    """Run a command, which must succeed.

    Returns:
        The command's standard output.
    """
    return subprocess.run(cmd, check=True, capture_output=True, text=True).stdout


def succeeds(cmd):
    """Run a command, discarding its output.

    Returns:
        Whether the command succeeded.
    """
    return subprocess.run(cmd, check=False, capture_output=True).returncode == 0


def and_list(items):
    """Join items as an English list, such as "a, b, and c".

    Returns:
        The items, separated by commas and "and".
    """
    items = list(items)
    if len(items) <= 2:
        return " and ".join(items)
    return ", ".join(items[:-1]) + ", and " + items[-1]


def basename(path):
    """Return the last component of a path.

    Returns:
        The part of path after its last "/", ignoring a trailing "/".
    """
    return path.rstrip("/").split("/")[-1]


class Repo:
    """A GitHub repository, its open PRs, and the merge analysis of those PRs."""

    def __init__(self, cfg):
        """Create a Repo from its entry in the config, with no PRs."""
        self.name = cfg["name"]
        self.gh = cfg["gh"]
        self.clone = cfg["clone"]
        self.prefix = cfg["prefix"]
        self.label = cfg["label"]
        self.node_prefix = cfg["node_prefix"]
        self.owner = self.gh.split("/")[0]
        self.dir_name = cfg.get("dir_name", self.name)
        self.default_branch = cfg.get("default_branch", "master")
        self.keywords = cfg.get("dependency_keywords", [])
        self.show_author = cfg.get("show_author", False)
        self.prs = {}  # number -> gh JSON
        self.master_conflicts = {}  # number -> list of files
        self.contains = set()  # (a, b): a contains all non-merge commits of b
        self.pair_conflicts = {}  # (a, b) with a < b -> list of files
        self.closed_state = {}  # number -> "landed" | "closed" | None (not a PR)

    def ref(self, n):
        """Return how documents refer to PR n.

        Returns:
            The PR's number, with the repository's prefix, such as "fjalar#95".
        """
        return f"{self.prefix}{n}"

    def link(self, n):
        """Return a Markdown link to PR n.

        Returns:
            A link whose text is the PR's reference and whose target is the PR on GitHub.
        """
        return f"[{self.ref(n)}](https://github.com/{self.gh}/pull/{n})"

    def refs(self, ns):
        """Return how documents refer to the PRs ns.

        Returns:
            The PRs' references, in numeric order, as an English list.
        """
        return and_list(self.ref(n) for n in sorted(ns))

    def state_of(self, n):
        """Return the state of PR n, which is not open.

        Returns:
            "landed", "closed", or None (for an issue or a nonexistent number).
        """
        if n not in self.closed_state:
            try:
                j = json.loads(run(["gh", "pr", "view", str(n), "-R", self.gh, "--json", "state"]))
                self.closed_state[n] = {"MERGED": "landed", "CLOSED": "closed"}.get(j["state"])
            except subprocess.CalledProcessError:
                self.closed_state[n] = None
        return self.closed_state[n]


def load_repo(repo, pairwise=True):
    """Read the open PRs and the merge analysis.

    Without pairwise, repo.pair_conflicts stays empty.
    """
    fields = (
        "number,title,headRefName,headRefOid,headRepositoryOwner,baseRefName,"
        "author,assignees,body,updatedAt"
    )
    for pr in json.loads(
        run(
            [
                "gh",
                "pr",
                "list",
                "-R",
                repo.gh,
                "--state",
                "open",
                "--limit",
                "300",
                "--json",
                fields,
            ]
        )
    ):
        pr["body"] = pr["body"].replace("\r", "")
        pr["title"] = pr["title"].strip()
        pr["owner"] = (pr.get("headRepositoryOwner") or {}).get("login", "")
        repo.prs[pr["number"]] = pr
    if not repo.prs:
        return
    out = run(
        [
            ANALYSIS,
            *([] if pairwise else ["--no-pairwise"]),
            repo.clone,
            repo.gh,
            *map(str, sorted(repo.prs)),
        ]
    )
    section = None
    for line in out.splitlines():
        if line.startswith("## "):
            section = line.split()[1]
            continue
        if section == "master":
            n, files = line.split(":", 1)
            repo.master_conflicts[int(n)] = files.split()
        elif section == "containment":
            a, _, b = line.split()
            repo.contains.add((int(a), int(b)))
        elif section == "pairwise":
            m = re.match(r"(\d+) vs (\d+):(.*)", line)
            assert m, line
            repo.pair_conflicts[int(m[1]), int(m[2])] = m[3].split()


PR_STATES = {"OPEN": "open", "MERGED": "landed", "CLOSED": "closed"}


EXTERNAL_STATE = {}


def external_state(ref):
    """Return the state of a PR "owner/repo#N" in another repository.

    Returns:
        "open", "landed", "closed", or None.
    """
    if ref not in EXTERNAL_STATE:
        gh, n = ref.split("#")
        try:
            j = json.loads(run(["gh", "pr", "view", n, "-R", gh, "--json", "state"]))
            EXTERNAL_STATE[ref] = PR_STATES.get(j["state"])
        except subprocess.CalledProcessError:
            EXTERNAL_STATE[ref] = None
    return EXTERNAL_STATE[ref]


def query_pr_states(refs):
    """Look up the states of the PRs "owner/repo#N" in refs, with one GraphQL query.

    Returns:
        A map from each ref to "open", "landed", "closed", or None (not a PR),
        or {} if the query fails.
    """
    refs = sorted(set(refs))
    if not refs:
        return {}
    query = []
    for i, ref in enumerate(refs):
        gh, n = ref.split("#")
        owner, name = gh.split("/")
        query.append(
            f"r{i}: repository(owner: {json.dumps(owner)}, name: {json.dumps(name)}) "
            f"{{ pullRequest(number: {int(n)}) {{ state }} }}"
        )
    # A ref that is not a PR yields an error but no output; the other refs still yield data.
    result = subprocess.run(
        ["gh", "api", "graphql", "-f", "query={" + " ".join(query) + "}"],
        check=False,
        capture_output=True,
        text=True,
    )
    try:
        data = json.loads(result.stdout).get("data")
    except json.JSONDecodeError:
        data = None
    if not data:
        return {}
    states = {}
    for i, ref in enumerate(refs):
        pr = (data.get(f"r{i}") or {}).get("pullRequest")
        states[ref] = PR_STATES.get(pr["state"]) if pr else None
    return states


BASE_KEYWORDS = ["depends on", "depend on", "builds on", "land", "merge"]

LANDING_LINE = re.compile(r"Land (after|with|before) #(\d+)\.[ \t]*")


def pr_refs(text, repo):
    """Find the PRs that text names.

    Returns:
        A set of numbers in repo, and a set of "owner/repo#N" in other repositories.
    """
    numbers = {int(n) for n in re.findall(r"(?<![\w/])#(\d+)\b", text)}
    external = set()
    for gh, n in re.findall(r"\b([\w.-]+/[\w.-]+)#(\d+)\b", text) + re.findall(
        r"github\.com/([\w.-]+/[\w.-]+)/pull/(\d+)", text
    ):
        if gh == repo.gh:
            numbers.add(int(n))
        else:
            external.add(f"{gh}#{n}")
    return numbers, external


def stated_prerequisites(body, repo):
    """Find the PRs that a description names as prerequisites or as dependents of its PR.

    Returns:
        A set of numbers in repo and a set of "owner/repo#N" in other repositories, which
        are prerequisites, and a set of numbers in repo, which are dependents.
    """
    keywords = BASE_KEYWORDS + repo.keywords
    pattern = re.compile(r"\b(" + "|".join(re.escape(k) for k in keywords) + r")\b", re.IGNORECASE)
    numbers, external, dependents = set(), set(), set()
    # A colon does not end a sentence, so "Merge with:" and the list that follows stay together.
    for sentence in re.split(r"(?<=[.;])\s+|\n\s*\n(?!\s*https?://)", body):
        for m in pattern.finditer(sentence):
            rest = sentence[m.end() :]
            keyword = m[1].lower()
            if keyword not in ("land", "merge"):
                if re.match(r"\s*(this|it)\b", rest, re.IGNORECASE):
                    continue  # "#N depends on this PR":  #N is a dependent
                ns, ext = pr_refs(rest, repo)
                numbers |= ns
                external |= ext
                continue
            if not (
                re.search(r"\b(first|after)\b", rest, re.IGNORECASE)
                or re.match(
                    r"\s*((this|it)\s+)?(together with|with|right before|before)[\s:]",
                    rest,
                    re.IGNORECASE,
                )
            ):
                continue
            # In "Land it after #A and before #B", #A is a prerequisite and #B is a dependent.
            parts = re.split(r"\b(after|before|with)\b", rest, flags=re.IGNORECASE)
            for i in range(0, len(parts), 2):
                ns, ext = pr_refs(parts[i], repo)
                if i and parts[i - 1].lower() == "before":
                    dependents |= ns
                else:
                    numbers |= ns
                    external |= ext
    return numbers, external, dependents


def updated_pr_description(body, prerequisites, dependents):
    """Update the conventional landing-order lines in a PR description.

    Existing ``Land with #N.`` lines are retained between the generated "Land after"
    and "Land before" groups.

    Returns:
        The updated description.
    """
    lines = body.splitlines()
    final_newline = body.endswith("\n")
    landing = [(i, LANDING_LINE.fullmatch(line)) for i, line in enumerate(lines)]
    landing = [(i, match) for i, match in landing if match]
    with_lines = [lines[i] for i, match in landing if match[1] == "with"]
    with_prs = {int(match[2]) for _, match in landing if match[1] == "with"}
    replacement = [
        *(f"Land after #{n}." for n in sorted(set(prerequisites) - with_prs)),
        *with_lines,
        *(f"Land before #{n}." for n in sorted(set(dependents) - with_prs)),
    ]

    if landing:
        landing_indices = {i for i, _ in landing}
        insertion = landing[0][0]
        lines = [line for i, line in enumerate(lines) if i not in landing_indices]
        lines[insertion:insertion] = replacement
        if not replacement:
            # Remove one blank line that only separated the landing-order block from the body.
            if insertion == len(lines) and insertion and not lines[insertion - 1]:
                lines.pop()
            elif insertion == 0 and lines and not lines[0]:
                lines.pop(0)
            elif 0 < insertion < len(lines) and not lines[insertion - 1] and not lines[insertion]:
                lines.pop(insertion)
    elif replacement:
        if lines and lines[-1]:
            lines.append("")
        lines.extend(replacement)
    else:
        return body

    return "\n".join(lines) + ("\n" if final_newline else "")


class Model:
    """The dependencies among the open PRs of a repository."""

    def __init__(self, repo, cfg):
        """Compute the dependencies among the open PRs of repo, which load_repo has read."""
        self.repo = repo
        rc = cfg["repos"][repo.name]
        self.excluded = set(rc.get("excluded", {}).get("prs", []))
        not_prereq = set(rc.get("not_prerequisites", []))
        open_prs = set(repo.prs) - self.excluded
        self.open = open_prs
        self.stated = {}
        self.stated_dependents = {}
        self.stated_closed = {}
        self.external = {}
        prereqs = {n: stated_prerequisites(repo.prs[n]["body"], repo) for n in open_prs}
        # Look up all the states at once; state_of and external_state look up any that remain.
        external = {e for _, ext, _ in prereqs.values() for e in ext} | {
            e for refs in rc.get("extra_external_dependencies", {}).values() for e in refs
        }
        internal = {
            f"{repo.gh}#{m}" for names, _, _ in prereqs.values() for m in names - set(repo.prs)
        }
        for ref, state in query_pr_states((external - set(EXTERNAL_STATE)) | internal).items():
            if ref in external:
                EXTERNAL_STATE[ref] = state
            else:
                repo.closed_state[int(ref.split("#")[1])] = state if state != "open" else None
        for n in open_prs:
            names, external, dependents = prereqs[n]
            names -= {n}
            self.stated[n] = names & open_prs
            self.stated_dependents[n] = (dependents - {n}) & open_prs
            self.stated_closed[n] = sorted(
                (m, repo.state_of(m)) for m in names - set(repo.prs) if repo.state_of(m)
            )
            self.external[n] = sorted(e for e in external if external_state(e) == "open")
        for n, refs in rc.get("extra_external_dependencies", {}).items():
            if int(n) in open_prs:
                self.external[int(n)] = sorted(
                    set(self.external[int(n)]) | {e for e in refs if external_state(e) == "open"}
                )
        deps = defaultdict(set)
        for n in open_prs:
            deps[n] |= self.stated[n]
            for m in self.stated_dependents[n]:
                deps[m].add(n)
        # A stacked PR depends on the PR whose branch is its base branch.
        self.base_pr = {}
        by_head = {(pr["owner"], pr["headRefName"]): m for m, pr in repo.prs.items()}
        for n in open_prs:
            base = repo.prs[n]["baseRefName"]
            if base != repo.default_branch:
                m = by_head.get((repo.owner, base))
                if m in open_prs:
                    deps[n].add(m)
                    self.base_pr[n] = m
        for a, b in repo.contains:
            if (
                a in open_prs
                and b in open_prs
                and b not in not_prereq
                and (b, a) not in repo.contains
            ):
                deps[a].add(b)
        self.extra = defaultdict(set)
        for a, b in rc.get("extra_dependencies", []):
            if a in open_prs and b in open_prs:
                deps[a].add(b)
                self.extra[a].add(b)
        self.all_deps = {n: self.closure(deps, n) for n in open_prs}
        # Each PR that depends on itself, such as two PRs whose descriptions say to land each
        # with the other.
        self.cyclic = sorted(n for n in open_prs if any(n in self.all_deps[d] for d in deps[n]))
        # Direct dependencies:  the transitive reduction.
        self.direct = {}
        for n in open_prs:
            indirect = (
                set().union(*(self.all_deps[d] for d in self.all_deps[n]))
                if self.all_deps[n]
                else set()
            )
            self.direct[n] = self.all_deps[n] - indirect
        self.dependents = defaultdict(set)
        for n in open_prs:
            for d in self.direct[n]:
                self.dependents[d].add(n)
        self.all_dependents = {n: {m for m in open_prs if n in self.all_deps[m]} for n in open_prs}

    @staticmethod
    def closure(deps, n):
        """Compute the transitive dependencies of n.

        Returns:
            Every PR that n depends on, directly or transitively, according to deps.
        """
        seen, todo = set(), list(deps[n])
        while todo:
            d = todo.pop()
            if d not in seen and d != n:
                seen.add(d)
                todo.extend(deps[d])
        return seen

    def in_graph(self, n):
        """Return whether PR n depends on, or is depended on by, another open PR.

        Returns:
            True if PR n has a direct prerequisite or a direct dependent.
        """
        return bool(self.direct[n] or self.dependents[n])

    def unblocked(self):
        """Return the PRs that depend on no other open PR.

        Returns:
            The sorted numbers of the PRs with no prerequisite, in this or another repository.
        """
        return sorted(n for n in self.open if not self.direct[n] and not self.external[n])

    def blocked(self):
        """Return the PRs that depend on another open PR.

        Returns:
            The sorted numbers of the PRs with a prerequisite, in this or another repository.
        """
        return sorted(n for n in self.open if self.direct[n] or self.external[n])

    def independent(self):
        """Return the PRs that have no prerequisite and no dependent.

        Returns:
            The sorted numbers of the PRs that are unrelated to every other open PR.
        """
        return sorted(n for n in self.open if not self.in_graph(n) and not self.external[n])

    def assignees(self, n):
        """Return the assignees of PR n.

        Returns:
            The GitHub logins of the PR's assignees.
        """
        return [a["login"] for a in self.repo.prs[n]["assignees"]]

    def assigned_to(self, n):
        """Return the "Assigned to" cell of PR n.

        Returns:
            The PR's assignees, as an English list.
        """
        return and_list(self.assignees(n))

    def head_status(self, a, b):
        """Return whether a's branch has b's current commits.

        Returns:
            "yes", "newest" (a lacks only b's newest commits), or "none".
        """
        g = ["git", "-C", self.repo.clone]
        pa, pb = f"refs/remotes/pr/{a}", f"refs/remotes/pr/{b}"
        if succeeds([*g, "merge-base", "--is-ancestor", pb, pa]):
            return "yes"
        missing = run(
            [*g, "rev-list", "--no-merges", pb, f"^{pa}", "^refs/remotes/csm/master"]
        ).split()
        if not missing:
            return "yes"  # a lacks only merge commits of b
        own = run([*g, "rev-list", "--no-merges", pb, "^refs/remotes/csm/master"]).split()
        return "none" if len(missing) == len(own) else "newest"

    def levels(self):
        """Compute each PR's step in the merge order.

        Returns:
            A map from each PR in the dependency graph to 1 plus the largest level of its
            direct prerequisites, or to 1 if it has none.
        """
        level = {}

        def lev(n):
            if n not in level:
                level[n] = 1  # provisional, so that a cycle of dependencies terminates
                level[n] = 1 + max((lev(d) for d in self.direct[n]), default=0)
            return level[n]

        for n in self.open:
            if self.in_graph(n):
                lev(n)
        return level

    def components(self):
        """Compute the connected components of the dependency graph.

        Returns:
            The components, each a sorted list of PR numbers, ordered by their smallest PR.
        """
        nodes = sorted(n for n in self.open if self.in_graph(n))
        seen, comps = set(), []
        for n in nodes:
            if n in seen:
                continue
            comp, todo = set(), [n]
            while todo:
                m = todo.pop()
                if m in comp:
                    continue
                comp.add(m)
                todo.extend(self.direct[m] | self.dependents[m])
            seen |= comp
            comps.append(sorted(comp))
        return comps


def description_updates(models):
    """Find the PR descriptions whose conventional landing-order lines are stale.

    Returns:
        Tuples of the model, PR number, and updated description.
    """
    updates = []
    for model in models:
        with_prs = defaultdict(set)
        for n in model.open:
            for line in model.repo.prs[n]["body"].splitlines():
                match = LANDING_LINE.fullmatch(line)
                if match and match[1] == "with" and int(match[2]) in model.open:
                    other = int(match[2])
                    with_prs[n].add(other)
                    with_prs[other].add(n)
        for n in sorted(model.open):
            body = model.repo.prs[n]["body"]
            updated = updated_pr_description(
                body, model.direct[n] - with_prs[n], model.dependents[n] - with_prs[n]
            )
            if updated != body:
                updates.append((model, n, updated))
    return updates


def refresh_description_facts(model, cfg, n):
    """Refresh the facts derived from one locally updated PR description."""
    r = model.repo
    names, external, dependents = stated_prerequisites(r.prs[n]["body"], r)
    names -= {n}
    model.stated[n] = names & model.open
    model.stated_dependents[n] = (dependents - {n}) & model.open
    model.stated_closed[n] = sorted(
        (m, state) for m in names - set(r.prs) if (state := r.state_of(m))
    )
    configured = cfg["repos"][r.name].get("extra_external_dependencies", {}).get(str(n), [])
    model.external[n] = sorted(
        {ref for ref in external | set(configured) if external_state(ref) == "open"}
    )


def apply_description_updates(updates, cfg):
    """Write PR description updates to GitHub and reflect them in the in-memory facts.

    Returns:
        The updates that GitHub rejected, paired with their errors.
    """
    failures = []
    for model, n, body in updates:
        r = model.repo
        try:
            run(["gh", "pr", "edit", str(n), "-R", r.gh, "--body", body])
        except subprocess.CalledProcessError as error:
            failures.append((model, n, error))
            continue
        r.prs[n]["body"] = body
        refresh_description_facts(model, cfg, n)
    return failures


def needed_by(model, n):
    """Return the "Needed by" cell of PR n.

    Returns:
        The PR's direct dependents, followed by its transitive ones, or "" if it has none.
    """
    direct = model.dependents[n]
    if not direct:
        return ""
    r = model.repo
    text = r.refs(direct)
    more = model.all_dependents[n] - direct
    if more:
        text += f" (and transitively {r.refs(more)})"
    return text


def depends_on(model, n):
    """Return the "Depends on" cell of PR n.

    Returns:
        The PR's direct prerequisites, followed by its transitive ones.
    """
    r = model.repo
    items = [r.ref(d) for d in sorted(model.direct[n])] + model.external[n]
    text = and_list(items)
    more = model.all_deps[n] - model.direct[n]
    if more:
        text += f" (and transitively {r.refs(more)})"
    return text


def auto_notes(model, n):
    """Generate the notes about PR n that do not come from the config.

    Returns:
        Note fragments, each starting with a lowercase letter.
    """
    r = model.repo
    notes = []
    if n in model.base_pr:
        notes.append(f"its base branch is {r.ref(model.base_pr[n])}'s branch")
    landed = [m for m, s in model.stated_closed[n] if s == "landed"]
    if landed:
        notes.append(
            f"its description still names the landed {r.refs(landed)} as "
            + ("a prerequisite" if len(landed) == 1 else "prerequisites")
        )
    if n in r.master_conflicts:
        notes.append(
            "conflicts with `master` in " + and_list(f"`{f}`" for f in r.master_conflicts[n])
        )
    return notes


def fix_empty_cells(lines):
    """Write each empty table cell as "| |", not "|  |".

    Returns:
        The lines, with their empty table cells rewritten.
    """
    return [
        re.sub(r"(?<=\|)  (?=\|)", " ", line) if line.startswith("|") else line for line in lines
    ]


def table_row(cells):
    """Return a Markdown table row containing cells.

    Pipe characters within a cell are escaped so that they do not create extra columns.

    Returns:
        A Markdown table row.
    """
    return "| " + " | ".join(re.sub(r"(?<!\\)\|", r"\\|", cell) for cell in cells) + " |"


def capitalize(s):
    """Capitalize the first letter of s, leaving the rest of s unchanged.

    Returns:
        s, with its first letter in uppercase.
    """
    return s[:1].upper() + s[1:] if s else s


def unblocked_notes(model, cfg, n):
    """Return the "Notes" cell of PR n in a table of unblocked PRs.

    Returns:
        The PR's notes from the config, followed by the generated ones.
    """
    manual = cfg["repos"][model.repo.name].get("unblocked_notes", {}).get(str(n), "")
    parts = ([manual] if manual else []) + auto_notes(model, n)
    return capitalize("; ".join(parts))


def detail_notes(model, cfg, n):
    """Return the "Notes" cell of PR n in a table of PRs that have dependencies.

    Returns:
        The PR's notes from the config, followed by the generated ones, as sentences.
    """
    manual = cfg["repos"][model.repo.name].get("detail_notes", {}).get(str(n), "")
    parts = ([manual] if manual else []) + [capitalize(s) + "." for s in auto_notes(model, n)]
    return "  ".join(parts)


def title(model, cfg, n):
    """Return the title of PR n.

    Returns:
        The PR's title from the config, or else from GitHub.
    """
    return cfg["repos"][model.repo.name].get("titles", {}).get(str(n), model.repo.prs[n]["title"])


DIRECTORIES = None


def scan_directories(base):
    """Classify each entry of base, once.

    Owner and repo are lowercase; branch is the current branch, or "" if HEAD is detached.

    Returns:
        A map from each entry of base to ("link", target), ("git", owner, repo, branch),
        or ("plain",).
    """
    global DIRECTORIES
    if DIRECTORIES is None:
        DIRECTORIES = {}
        for path in sorted(pathlib.Path(base).iterdir()):
            d = path.name
            if d.startswith(".") or not (path.is_dir() or path.is_symlink()):
                continue
            if path.is_symlink():
                DIRECTORIES[d] = ("link", str(path.readlink()))
                continue
            try:
                url = run(["git", "-C", str(path), "remote", "get-url", "origin"]).strip()
                branch = run(["git", "-C", str(path), "branch", "--show-current"]).strip()
            except subprocess.CalledProcessError:
                DIRECTORIES[d] = ("plain",)
                continue
            m = re.search(r"github\.com[:/]+([^/]+)/(.+?)(?:\.git)?/?$", url)
            if m:
                DIRECTORIES[d] = ("git", m[1].lower(), m[2].lower(), branch)
            else:
                DIRECTORIES[d] = ("git", "", url, branch)
    return DIRECTORIES


def pr_directories(repo, pr, base):
    """Find the working copies of a PR's branch.

    Returns:
        The entries of base whose current branch is the PR's head branch and whose origin is
        the PR's head repository or the upstream repository.
    """
    owner, name = repo.gh.lower().split("/")
    result = []
    for d, info in scan_directories(base).items():
        if (
            info[0] == "git"
            and info[2] == name
            and info[3] == pr["headRefName"]
            and (
                info[1] == pr["owner"].lower()
                or (info[1] == owner and info[3] != repo.default_branch)
            )
        ):
            result.append(d)
    return result


def pr_directory(repo, n, base):
    """Return the "Directory" cell of PR n.

    Returns:
        The PR's working copies, or "(none)".
    """
    dirs = pr_directories(repo, repo.prs[n], base)
    return ", ".join(f"`{base}/{d}/`" for d in dirs) if dirs else "(none)"


# ---------- unblocked documents ----------


def keep(model, n, flt):
    """Return whether PR n passes a document's filter.

    Returns:
        True if the document should list PR n.
    """
    who = model.assignees(n)
    if flt == "all":
        return True
    if flt == "unassigned":
        return not who
    return not who or any(w in flt["unassigned_or"] for w in who)


def unblocked_doc(models, cfg, base, doc, as_of):
    """Generate an unblocked document.

    Returns:
        The text of the document that doc describes.
    """
    flt = doc.get("filter", "all")
    show_author = any(m.repo.show_author for m in models)
    lines = [
        f"# {doc['title']}",
        "",
        'An "unblocked" or "nonblocked" pull request is one that depends on no other PR.',
        "",
        f"As of {as_of}.",
        (
            "Generated by `~/bin/share/pr-status.py` from GitHub and `pr-status-config.json`;"
            " do not edit it by hand."
        ),
        f"See `{cfg['documents']['dependencies']}` for how dependencies are determined.",
        "Each of these PRs can land without first landing any other open PR.",
    ]
    if flt == "unassigned":
        lines.append("No one is assigned to any of these PRs.")
    elif flt != "all":
        lines.append(
            f"Each of these PRs is unassigned or assigned to {and_list(flt['unassigned_or'])}."
        )
    lines.append('Some of them are prerequisites of other PRs (see "Needed by").')
    for m in models:
        r = m.repo
        heading = [
            "PR",
            *(["Author"] if show_author else []),
            "Assigned to",
            "Title",
            "Directory",
            "Needed by",
            "Notes",
        ]
        lines += [
            "",
            *([f"## {r.label}", ""] if len(models) > 1 else []),
            table_row(heading),
            "|" + " --- |" * len(heading),
        ]
        for n in m.unblocked():
            if not keep(m, n, flt):
                continue
            author = [r.prs[n]["author"]["login"]] if show_author else []
            cells = [
                r.link(n),
                *author,
                m.assigned_to(n),
                title(m, cfg, n),
                pr_directory(r, n, base),
                needed_by(m, n),
                unblocked_notes(m, cfg, n),
            ]
            lines.append(table_row(cells))
    excluded = [(m, cfg["repos"][m.repo.name].get("excluded", {})) for m in models]
    if any(ex.get("prs") for _, ex in excluded):
        lines += ["", "## Excluded by decision", ""]
        for m, ex in excluded:
            if ex.get("prs"):
                lines.append(f"* {m.repo.label}: {ex['reason']}")
    lines += ["", "## Excluded because they depend on another PR", ""]
    for m in models:
        items = []
        for n in m.blocked():
            on = [m.repo.ref(d) for d in sorted(m.direct[n])] + m.external[n]
            items.append(f"{m.repo.ref(n)} (on {', '.join(on)})")
        if items:
            lines.append(f"* {m.repo.label}: {', '.join(items)}.")
    if flt != "all":
        lines += [
            "",
            "## Excluded because they are assigned to someone"
            + ("" if flt == "unassigned" else " else"),
            "",
        ]
        for m in models:
            by = defaultdict(list)
            for n in m.unblocked():
                if not keep(m, n, flt):
                    by[", ".join(m.assignees(n))].append(n)
            if by:
                groups = [f"{m.repo.refs(ns)} ({who})" for who, ns in sorted(by.items())]
                lines.append(f"* {m.repo.label}: {'; '.join(groups)}.")
    return "\n".join(fix_empty_cells(lines)) + "\n"


# ---------- generated sections of the dependency document ----------


def gen_status(models):
    """Generate the "status" section.

    Returns:
        The section's lines:  which PRs conflict with `master`, and which dependent branches
        lack their prerequisites' current commits.
    """
    lines = []
    for m in models:
        r = m.repo
        conflicting = sorted(r.master_conflicts)
        if conflicting:
            lines.append(
                f"* Every {r.label} PR merges with `master` cleanly, except {r.refs(conflicting)}."
            )
        else:
            lines.append(f"* Every {r.label} PR merges with `master` cleanly.")
    where = (
        ", in both repositories,"
        if len(models) == 2
        else (", in every repository," if len(models) > 2 else "")
    )
    behind = []
    for m in models:
        r = m.repo
        for n in sorted(m.open):
            newest = [d for d in sorted(m.direct[n]) if m.head_status(n, d) == "newest"]
            none = [d for d in sorted(m.direct[n]) if m.head_status(n, d) == "none"]
            parts = []
            if newest:
                parts.append(
                    f"the newest commits of {r.refs(newest)}"
                    if len(newest) > 1
                    else f"{r.ref(newest[0])}'s newest commits"
                )
            if none:
                parts.append(f"every commit of {r.refs(none)}")
            if parts:
                behind.append(f"  * {r.ref(n)} lacks {and_list(parts)}.")
    contains = (
        f"* Every dependent branch{where} contains its immediate prerequisites' current commits"
    )
    if behind:
        lines.append(contains + ", except:")
        lines += behind
    else:
        lines.append(contains + ".")
    return lines


def gen_clusters(models, cfg):
    """Generate the "clusters" section.

    Returns:
        The section's lines:  each repository's connected components of the dependency graph.
    """
    lines = []
    for m in models:
        names = cfg["repos"][m.repo.name].get("cluster_names", {})
        lines.append(f"* {m.repo.label} has these dependency clusters:")
        for comp in m.components():
            name = next((names[str(n)] for n in comp if str(n) in names), None)
            members = m.repo.refs(comp)
            lines.append(f"  * {name} ({members})" if name else f"  * {members}")
    return lines


def gen_graph(models, cfg):
    """Generate the "graph" section.

    Returns:
        The section's lines:  a Mermaid graph of the dependencies.
    """
    lines = ["```mermaid", "graph BT"]
    for m in models:
        r = m.repo
        labels = cfg["repos"][r.name].get("labels", {})
        names = cfg["repos"][r.name].get("cluster_names", {})
        for comp in m.components():
            name = next((names[str(n)] for n in comp if str(n) in names), None)
            lines.append(f"  %% {r.label}: {name or r.refs(comp)}")
            for n in comp:
                label = labels.get(str(n)) or r.prs[n]["title"][:50]
                label = label.replace('"', "'")
                lines.append(f'  {r.node_prefix}{n}["{r.ref(n)} {label}"]')
            for n in comp:
                lines.extend(
                    f"  {r.node_prefix}{d} --> {r.node_prefix}{n}" for d in sorted(m.direct[n])
                )
            lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("```")
    return lines


def gen_merge_order(models):
    """Generate the "merge-order" section.

    Returns:
        The section's lines:  for each repository, the PRs grouped by their level.
    """
    lines = []
    for i, m in enumerate(models):
        r = m.repo
        level = m.levels()
        if i:
            lines += [f"{r.label} (independent of the {models[0].repo.label} order):", ""]
        else:
            lines += [f"{r.label}:", ""]
        for k in range(1, max(level.values(), default=0) + 1):
            prs = sorted(n for n, v in level.items() if v == k)
            extra = " (and any independent PR)" if k == 1 else ""
            lines.append(f"{k}. {', '.join(r.ref(n) for n in prs)}{extra}")
        lines.append("")
    if lines and lines[-1] == "":
        lines.pop()
    return lines


def dependency_cells(model, n):
    """Return the dependency cells of PR n, which has a prerequisite.

    Returns:
        The "Depends on", "Stated in description", and
        "Branch contains prerequisites' commits" cells.
    """
    r = model.repo
    direct = sorted(model.direct[n])
    stated = [
        r.ref(d) for d in direct if d in model.stated[n] or n in model.stated_dependents[d]
    ] + model.external[n]
    unstated = [
        r.ref(d) for d in direct if d not in model.stated[n] and n not in model.stated_dependents[d]
    ]
    if not unstated:
        stated_text = "yes"
    elif not stated:
        stated_text = "no"
    else:
        stated_text = f"{and_list(stated)}:  yes; {and_list(unstated)}:  no"
    status = {d: model.head_status(n, d) for d in direct}
    newest = [d for d in direct if status[d] == "newest"]
    none = [d for d in direct if status[d] == "none"]
    if not direct:
        contains = "not applicable"
    elif not newest and not none:
        contains = "yes"
    else:
        parts = []
        if newest:
            parts.append(
                f"lacks the newest commits of {r.refs(newest)}"
                if len(newest) > 1
                else f"lacks {r.ref(newest[0])}'s newest commits"
            )
        if none:
            parts.append(f"lacks every commit of {r.refs(none)}")
        contains = f"no ({'; '.join(parts)})"
    return [depends_on(model, n), stated_text, contains]


def gen_details(model, cfg):
    """Generate the "details-REPO" section.

    Returns:
        The section's lines:  a table of the PRs in the dependency graph.
    """
    r = model.repo
    lines = [
        (
            "| PR | Assigned to | Depends on | Stated in description"
            " | Branch contains prerequisites' commits | Notes |"
        ),
        "| --- | --- | --- | --- | --- | --- |",
    ]
    for n in sorted(model.open):
        if not model.in_graph(n) and not model.external[n]:
            continue
        t = title(model, cfg, n)
        if not model.direct[n] and not model.external[n]:
            cells = ["none", "not applicable", "not applicable"]
        else:
            cells = dependency_cells(model, n)
        lines.append(
            table_row(
                [r.link(n) + " " + t, model.assigned_to(n), *cells, detail_notes(model, cfg, n)]
            )
        )
    return lines


def gen_prerequisites(model):
    """Generate the "prerequisites-REPO" section.

    Returns:
        The section's lines:  the prerequisites that have no prerequisite of their own.
    """
    r = model.repo
    items = []
    for n in sorted(model.open):
        if model.direct[n] or not model.dependents[n]:
            continue
        extra = [f"needed by {needed_by(model, n)}"]
        who = model.assignees(n)
        if who:
            extra.append("assigned to " + and_list(who))
        items.append(f"{r.ref(n)} ({'; '.join(extra)})")
    return [f"Prerequisites with no prerequisite of their own: {and_list(items)}."] if items else []


def textual_conflicts(model, cfg, n):
    """Return the "Textual conflicts" cell of PR n.

    Returns:
        The non-routine files in which PR n conflicts with other open PRs, grouped by
        partner, or "none".
    """
    r = model.repo
    routine = set(cfg["repos"][r.name].get("routine_files", []))
    by_file = defaultdict(set)
    for (a, b), files in r.pair_conflicts.items():
        if n not in (a, b):
            continue
        other = b if a == n else a
        if other in model.excluded:
            continue
        for f in files:
            if basename(f) not in routine:
                by_file[basename(f)].add(other)
    by_partners = defaultdict(list)
    for f, partners in by_file.items():
        by_partners[tuple(sorted(partners))].append(f)
    conflicts = "; ".join(
        f"{and_list(f'`{f}`' for f in sorted(files))} with {r.refs(partners)}"
        for partners, files in sorted(by_partners.items(), key=lambda kv: sorted(kv[1]))
    )
    return conflicts or "none"


def gen_independent(model, cfg):
    """Generate the "independent-REPO" section.

    Returns:
        The section's lines:  a table of the PRs that have no prerequisite and no dependent.
    """
    r = model.repo
    notes = cfg["repos"][r.name].get("independent_notes", {})
    lines = [
        "| PR | Assigned to | Title | Textual conflicts | Notes |",
        "| --- | --- | --- | --- | --- |",
    ]
    for n in model.independent():
        note = notes.get(str(n), "")
        auto = auto_notes(model, n)
        note = capitalize("; ".join(([note] if note else []) + auto))
        cells = [
            r.link(n),
            model.assigned_to(n),
            title(model, cfg, n),
            textual_conflicts(model, cfg, n),
            note,
        ]
        lines.append(table_row(cells))
    return lines


def pr_table(model, cfg, base, heading, rows):
    """Generate a table of PRs.

    The table's first columns are PR, Author (if the repository shows authors), Assigned to,
    Title, and Directory; heading names the remaining columns, and rows maps each PR to the
    cells of its remaining columns.

    Returns:
        The table's lines.
    """
    r = model.repo
    heading = [
        "PR",
        *(["Author"] if r.show_author else []),
        "Assigned to",
        "Title",
        "Directory",
        *heading,
    ]
    lines = [table_row(heading), "|" + " --- |" * len(heading)]
    for n, cells in rows:
        author = [r.prs[n]["author"]["login"]] if r.show_author else []
        row = [
            r.link(n),
            *author,
            model.assigned_to(n),
            title(model, cfg, n),
            pr_directory(r, n, base),
            *cells,
        ]
        lines.append(table_row(row))
    return lines


def gen_unblocked(model, cfg, base):
    """Generate the "unblocked-REPO" section.

    Returns:
        The section's lines:  a table of the PRs that depend on no other open PR.
    """
    return pr_table(
        model,
        cfg,
        base,
        ["Needed by", "Textual conflicts", "Notes"],
        [
            (
                n,
                [
                    needed_by(model, n),
                    textual_conflicts(model, cfg, n),
                    unblocked_notes(model, cfg, n),
                ],
            )
            for n in model.unblocked()
        ],
    )


def gen_blocked(model, cfg, base):
    """Generate the "blocked-REPO" section.

    Returns:
        The section's lines:  a table of the PRs that depend on another open PR.
    """
    return pr_table(
        model,
        cfg,
        base,
        ["Depends on", "Stated in description", "Branch contains prerequisites' commits", "Notes"],
        [(n, [*dependency_cells(model, n), detail_notes(model, cfg, n)]) for n in model.blocked()],
    )


def gen_directories(models, cfg, base):
    """Generate the "directories" section.

    Returns:
        The section's lines:  a table of the entries of base that are not a working copy of
        an open PR's branch.
    """
    dirs_info = scan_directories(base)
    working = set()
    for m in models:
        for pr in m.repo.prs.values():
            working.update(pr_directories(m.repo, pr, base))
    rows, covered = [], set()
    for row in cfg["directories"]:
        dirs = [d for d in row["dirs"] if d in dirs_info and d not in working]
        covered |= set(row["dirs"])
        if dirs:
            rows.append((dirs, row["status"], row["relationship"]))
    by_repo = {m.repo.gh.lower().split("/")[1]: m for m in models}
    groups = defaultdict(list)  # (status, relationship) -> directories
    landed = defaultdict(list)  # (repository, state) -> [(directory, ref)]
    for d, info in dirs_info.items():
        if d in working or d in covered:
            continue
        if info[0] == "link":
            groups[f"a symbolic link to `{info[1]}`", "none"].append(d)
        elif info[0] == "plain":
            groups["not a git repository", "none"].append(d)
        elif info[2] not in by_repo:
            where = f"{info[1]}/{info[2]}" if info[1] else info[2]
            groups[
                f"`{where}`, branch `{info[3]}`" if info[3] else f"`{where}`", "another repository"
            ].append(d)
        else:
            m = by_repo[info[2]]
            r = m.repo
            if info[3] in (r.default_branch, ""):
                state = "upstream `master`" if info[1] == r.owner.lower() else "a fork's `master`"
                groups[state, "none"].append(d)
                continue
            found = json.loads(
                run(
                    [
                        "gh",
                        "pr",
                        "list",
                        "-R",
                        r.gh,
                        "--state",
                        "all",
                        "--head",
                        info[3],
                        "--json",
                        "number,state,headRepositoryOwner",
                    ]
                )
            )
            found = [
                p
                for p in found
                if (p.get("headRepositoryOwner") or {}).get("login", "").lower()
                in (info[1], r.owner.lower())
            ]
            if not found:
                groups["a branch with no PR", "unknown"].append(d)
                continue
            p = max(found, key=operator.itemgetter("number"))
            state = {"MERGED": "landed", "CLOSED": "closed"}.get(p["state"], p["state"].lower())
            landed[r.name, state].append((d, r.ref(p["number"])))
    for (status, rel), dirs in groups.items():
        if status.startswith("a symbolic link"):
            rows += [([d], status, rel) for d in dirs]
        else:
            rows.append((dirs, status, rel))
    for (_, state), pairs in sorted(landed.items()):
        refs = [ref for _, ref in pairs]
        if state == "open":
            status = f"another copy of the branch of the open {and_list(refs)}"
        else:
            status = f"the {state} {and_list(refs)}"
        status += ", respectively" if len(refs) > 1 else ""
        rows.append(([d for d, _ in pairs], status, "none"))
    lines = ["| Directory | Status | Relationship to other branches |", "| --- | --- | --- |"]
    for dirs, status, rel in rows:
        lines.append(table_row([", ".join(f"`{d}`" for d in dirs), status, rel]))
    return lines


def replace_sections(text, generators):
    """Replace each generated section of text.

    generators maps a section name to a function that returns the section's lines.  A document
    need not contain every section.

    Returns:
        text, with the contents of each generated section regenerated.
    """
    pattern = re.compile(
        r"(<!-- BEGIN GENERATED: ([\w-]+) -->).*?(<!-- END GENERATED: \2 -->)", re.DOTALL
    )
    unknown = sorted({m[2] for m in pattern.finditer(text)} - set(generators))
    if unknown:
        sys.exit(
            "pr-status.py: unknown generated sections in the dependency document: "
            + ", ".join(unknown)
        )
    return pattern.sub(
        lambda m: "\n".join([m[1], *fix_empty_cells(generators[m[2]]()), m[3]]), text
    )


def section_generators(models, cfg, base):
    """Return the generators of the generated sections.

    Returns:
        A map from each generated section name to a function that returns the section's lines.
    """
    generators = {
        "status": lambda: gen_status(models),
        "clusters": lambda: gen_clusters(models, cfg),
        "graph": lambda: gen_graph(models, cfg),
        "merge-order": lambda: gen_merge_order(models),
        "directories": lambda: gen_directories(models, cfg, base),
        "push-commands": lambda: gen_push_commands(models, base),
    }
    if cfg["documents"].get("conflicting_push_commands"):
        generators["conflicting-push-commands"] = lambda: gen_push_commands(
            models, base, conflicts_only=True
        )
    for m in models:
        name = m.repo.name
        generators[f"details-{name}"] = lambda m=m: gen_details(m, cfg)
        generators[f"prerequisites-{name}"] = lambda m=m: gen_prerequisites(m)
        generators[f"independent-{name}"] = lambda m=m: gen_independent(m, cfg)
        generators[f"unblocked-{name}"] = lambda m=m: gen_unblocked(m, cfg, base)
        generators[f"blocked-{name}"] = lambda m=m: gen_blocked(m, cfg, base)
    return generators


# ---------- commands that update dependent branches ----------


def chain_has_conflict(repo, prs, has_upstream):
    """Return whether merging a chain of PRs would encounter a textual conflict.

    A later merge receives every earlier branch in the chain.  Therefore, a conflict
    between any 2 PRs in the chain can affect a merge, not only a conflict between 2
    adjacent PRs.  If the chain starts with upstream, every PR also receives upstream.

    Returns:
        True if at least one merge in the chain would conflict.
    """
    return (has_upstream and any(n in repo.master_conflicts for n in prs)) or any(
        tuple(sorted(pair)) in repo.pair_conflicts for pair in itertools.combinations(prs, 2)
    )


def push_chains(model, base, conflicts_only=False):
    """Compute the `git-push-to` chains that update every PR from upstream and its prerequisites.

    Each chain is a list of directories, each of which `git-push-to` pushes into the next.
    Each chain starts with a working copy of the upstream default branch, if there is one.
    A chain may contain a single PR, which has no direct prerequisite or dependent that has
    a working copy; such a chain exists only if there is a working copy of upstream.
    Before a chain pushes into a directory, the chains that precede it have updated that
    directory's prerequisites.
    If conflicts_only is true, retain a whole chain only if merging it would encounter a
    conflict between 2 of its PRs, or between upstream and one of its PRs.

    Returns:
        The chains, the PRs that have no working copy, and whether there is a working copy of
        the upstream default branch.
    """
    r = model.repo
    owner, name = r.gh.lower().split("/")
    upstream = [
        d
        for d, info in scan_directories(base).items()
        if info[0] == "git" and info[1:] == (owner, name, r.default_branch)
    ][:1]
    dirs = {n: pr_directories(r, r.prs[n], base) for n in model.open}
    missing = sorted(n for n in model.open if not dirs[n])
    edges = {(d, n) for n in model.open for d in model.direct[n] if dirs[d] and dirs[n]}
    in_edge = {n for e in edges for n in e}
    level = model.levels()
    pr_chains = []
    for d, n in sorted(edges, key=lambda e: (level[e[0]], e)):
        if (d, n) not in edges:
            continue
        edges.discard((d, n))
        chain = [d, n]
        # Extend the chain from n only if this chain has made n up to date.
        while len(model.direct[chain[-1]]) == 1:
            nxt = min((m for p, m in edges if p == chain[-1]), default=None)
            if nxt is None:
                break
            edges.discard((chain[-1], nxt))
            chain.append(nxt)
        pr_chains.append(chain)
    if upstream:
        pr_chains += [[n] for n in sorted(model.open) if dirs[n] and n not in in_edge]
    # A stable sort, which retains the order of the chains that start at the same level.
    # A PR that is not in the dependency graph has no level; it would be at level 1.
    pr_chains.sort(key=lambda chain: level.get(chain[0], 1))
    chains = [
        upstream + [dirs[n][0] for n in chain]
        for chain in pr_chains
        if not conflicts_only or chain_has_conflict(r, chain, bool(upstream))
    ]
    return chains, missing, bool(upstream)


def push_commands(models, base, conflicts_only=False):
    """Generate the shell commands that update each PR's branch from upstream and its prerequisites.

    Returns:
        The commands' lines, and notes about what the commands omit.
    """
    lines = [f"cd {base} && \\"]
    notes = []
    for m in models:
        r = m.repo
        chains, missing, has_upstream = push_chains(m, base, conflicts_only)
        for chain in chains:
            lines += ["git-push-to \\", *(f"  {d} \\" for d in chain[:-1]), f"  {chain[-1]}"]
            lines[-1] += " && \\"
        if missing:
            notes.append(
                f"{r.label} PRs with no working copy, which these commands omit: {r.refs(missing)}"
            )
        if chains and not has_upstream:
            notes.append(
                f"No working copy of upstream {r.label} `{r.default_branch}`,"
                " so these commands do not push it"
            )
    lines.append('echo "success"')
    return lines, notes


def gen_push_commands(models, base, conflicts_only=False):
    """Generate the "push-commands" section.

    Returns:
        The section's lines:  notes, then the commands in a fenced code block.
    """
    commands, notes = push_commands(models, base, conflicts_only)
    return [f"{note}." for note in notes] + ([""] if notes else []) + ["```sh", *commands, "```"]


# ---------- snapshot and changes ----------


def facts(models):
    """Collect the facts that the snapshot records.

    Returns:
        A map from each repository's name to its PRs, dependencies, conflicts with `master`,
        and unblocked PRs.
    """
    result = {}
    for m in models:
        r = m.repo
        result[r.name] = {
            "prs": {
                str(n): {
                    "title": pr["title"],
                    "head": pr["headRefOid"],
                    "assignees": sorted(m.assignees(n)) if n in m.open else [],
                    "body_sha": hashlib.sha256(pr["body"].encode()).hexdigest(),
                    "stated": sorted(m.stated.get(n, [])),
                }
                for n, pr in r.prs.items()
            },
            "direct": {str(n): sorted(m.direct[n]) for n in m.open},
            "master_conflicts": {str(n): fs for n, fs in r.master_conflicts.items()},
            "unblocked": m.unblocked(),
        }
    return result


def report_changes(models, old, new):
    """Compare the facts of the previous snapshot with the current ones.

    Returns:
        One line for each change.
    """
    out = []
    for m in models:
        r = m.repo
        o, n = old.get(r.name), new[r.name]
        if not o:
            out.append(f"{r.label}: no previous snapshot")
            continue
        op, np_ = o["prs"], n["prs"]
        out.extend(
            f"opened: {r.ref(k)} {np_[k]['title']}" for k in sorted(set(np_) - set(op), key=int)
        )
        for k in sorted(set(op) - set(np_), key=int):
            state = r.state_of(int(k)) or "gone"
            out.append(f"{state}: {r.ref(k)} {op[k]['title']}")
        for k in sorted(set(op) & set(np_), key=int):
            a, b = op[k], np_[k]
            ref = r.ref(k)
            if a["title"] != b["title"]:
                out.append(f"retitled: {ref}: {b['title']}")
            if a["assignees"] != b["assignees"]:
                out.append(f"assignees: {ref}: {a['assignees']} -> {b['assignees']}")
            if a["body_sha"] != b["body_sha"]:
                out.append(
                    f"description changed: {ref}"
                    + (
                        f" (stated prerequisites {a['stated']} -> {b['stated']})"
                        if a["stated"] != b["stated"]
                        else ""
                    )
                )
            if a["head"] != b["head"]:
                out.append(f"new commits: {ref}")
        od, nd = o["direct"], n["direct"]
        out.extend(
            f"dependencies: {r.ref(k)}: {od.get(k)} -> {nd[k]}"
            for k in sorted(set(od) | set(nd), key=int)
            if od.get(k) != nd.get(k) and k in nd
        )
        oc, nc = o["master_conflicts"], n["master_conflicts"]
        out.extend(
            f"master conflicts: {r.ref(k)}: {oc.get(k)} -> {nc[k]}"
            for k in sorted(nc, key=int)
            if oc.get(k) != nc[k]
        )
        if o["unblocked"] != n["unblocked"]:
            added = sorted(set(n["unblocked"]) - set(o["unblocked"]))
            removed = sorted(set(o["unblocked"]) - set(n["unblocked"]))
            if added:
                out.append(f"newly unblocked: {r.refs(added)}")
            if removed:
                out.append(f"no longer unblocked (blocked or gone): {r.refs(removed)}")
    return out


def main():
    """Report changes and regenerate the documents."""
    check_only = "--check" in sys.argv[1:]
    base = str(pathlib.Path.cwd())
    with pathlib.Path(CONFIG).open() as f:
        cfg = json.load(f)
    models = []
    for name in cfg["order"]:
        repo = Repo(cfg["repos"][name])
        load_repo(repo)
        models.append(Model(repo, cfg))
    updates = description_updates(models)
    failed_updates = []
    if not check_only:
        failed_updates = apply_description_updates(updates, cfg)
    now = dt.datetime.now().astimezone()
    as_of = now.strftime("%Y-%m-%d %H:%M %Z")
    new = facts(models)
    old = {}
    if pathlib.Path(SNAPSHOT).exists():
        with pathlib.Path(SNAPSHOT).open() as f:
            old = json.load(f)
    changes = report_changes(models, old.get("facts", {}), new)
    if changes:
        print(f"## Changes since the previous snapshot ({old.get('time', 'none')})")
        for line in changes:
            print(line)
    cycles = [m for m in models if m.cyclic]
    if cycles:
        print("## PRs that depend on themselves, through a cycle of dependencies")
        for m in cycles:
            print(m.repo.refs(m.cyclic))
    for m, n, error in failed_updates:
        detail = error.stderr.strip()
        print(
            f"pr-status.py: could not update {m.repo.ref(n)}" + (f": {detail}" if detail else ""),
            file=sys.stderr,
        )
    if failed_updates:
        stale_descriptions = [(m, n) for m in models for n in sorted(m.open) if m.stated_closed[n]]
        if stale_descriptions:
            print("## GitHub PR descriptions that name landed or closed PRs as prerequisites")
            for m, n in stale_descriptions:
                print(
                    f"{m.repo.ref(n)}: "
                    + ", ".join(f"{m.repo.ref(k)} ({s})" for k, s in m.stated_closed[n])
                )
        sys.exit(1)
    if updates and check_only:
        print("## GitHub PR descriptions need updates")
        for m, n, _ in updates:
            print(m.repo.ref(n))
    if check_only:
        return
    docs = cfg["documents"]
    for doc in docs["unblocked"]:
        pathlib.Path(doc["file"]).write_text(unblocked_doc(models, cfg, base, doc, as_of))
    text = pathlib.Path(docs["dependencies"]).read_text()
    text = replace_sections(text, section_generators(models, cfg, base))
    pathlib.Path(docs["dependencies"]).write_text(text)
    with pathlib.Path(SNAPSHOT).open("w") as f:
        json.dump({"time": now.isoformat(timespec="seconds"), "facts": new}, f, indent=1)
    print(
        f"## Wrote {', '.join(d['file'] for d in docs['unblocked'])}, the generated sections of "
        f"{docs['dependencies']}, and {SNAPSHOT}"
    )


if __name__ == "__main__":
    main()
