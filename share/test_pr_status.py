"""Tests for pr-status.py that do not access GitHub or local repositories."""

import importlib.machinery
import importlib.util
import sys
import types
import unittest
from pathlib import Path
from unittest import mock


def load_script() -> types.ModuleType:
    """Import pr-status.py, whose name is not a legal module name.

    Returns:
        The imported module.
    """
    path = Path(__file__).with_name("pr-status.py")
    loader = importlib.machinery.SourceFileLoader("pr_status", str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


pr_status = load_script()


class TableRowTest(unittest.TestCase):
    """Tests for table_row."""

    def test_escapes_pipes_within_cells(self):
        """A pipe within a cell does not create another Markdown table column."""
        self.assertEqual(
            pr_status.table_row(["Unbox && and ||", "second"]),
            r"| Unbox && and \|\| | second |",
        )
        self.assertEqual(pr_status.table_row([r"already \| escaped"]), r"| already \| escaped |")


class UpdatedPrDescriptionTest(unittest.TestCase):
    """Tests for updated_pr_description."""

    def test_adds_sorted_lines_after_body(self):
        """New landing-order lines follow the existing description."""
        self.assertEqual(
            pr_status.updated_pr_description("Description.", {30, 10}, {40, 20}),
            "Description.\n\nLand after #10.\nLand after #30.\nLand before #20.\nLand before #40.",
        )

    def test_replaces_lines_and_retains_land_with(self):
        """All 3 groups use the conventional order."""
        body = "Description.\n\nLand before #90.\nLand with #50.\nLand after #80.\n\nDetails.\n"
        self.assertEqual(
            pr_status.updated_pr_description(body, {20, 10}, {40, 30}),
            "Description.\n\nLand after #10.\nLand after #20.\nLand with #50.\n"
            "Land before #30.\nLand before #40.\n\nDetails.\n",
        )

    def test_land_with_takes_precedence(self):
        """A land-with relationship is not also written as a before/after relationship."""
        body = "Land with #20."
        self.assertEqual(
            pr_status.updated_pr_description(body, {10, 20}, {20, 30}),
            "Land after #10.\nLand with #20.\nLand before #30.",
        )

    def test_removes_obsolete_block_and_separator(self):
        """Removing a final landing-order block does not leave a blank line."""
        self.assertEqual(
            pr_status.updated_pr_description("Description.\n\nLand after #10.\n", set(), set()),
            "Description.\n",
        )

    def test_preserves_unconventional_prose(self):
        """Only complete conventional lines are managed."""
        body = "Please land after #10 because the API changes."
        self.assertEqual(pr_status.updated_pr_description(body, set(), set()), body)

    def test_idempotent(self):
        """An up-to-date description is unchanged."""
        body = "Description.\n\nLand after #10.\nLand with #20.\nLand before #30."
        self.assertEqual(pr_status.updated_pr_description(body, {10}, {30}), body)

    def test_land_with_takes_precedence_in_both_descriptions(self):
        """Land-with suppresses a generated line in the other PR's description."""
        repo = types.SimpleNamespace(prs={1: {"body": "Land with #2."}, 2: {"body": ""}})
        model = types.SimpleNamespace(
            repo=repo,
            open={1, 2},
            direct={1: {2}, 2: set()},
            dependents={1: set(), 2: {1}},
            unit={1: {1, 2}, 2: {1, 2}},
            external={1: [], 2: []},
        )
        self.assertEqual(pr_status.description_updates([model]), [])

    def test_replaces_external_prerequisites(self):
        """A PR in another repository that is no longer a prerequisite is removed."""
        body = "Land after other/repo#1592.\nLand after other/repo#1625.\n\nDescription.\n"
        self.assertEqual(
            pr_status.updated_pr_description(body, set(), set(), ["other/repo#1625"]),
            "Land after other/repo#1625.\n\nDescription.\n",
        )

    def test_adds_sorted_external_prerequisites(self):
        """External prerequisites follow the prerequisites in the PR's own repository."""
        self.assertEqual(
            pr_status.updated_pr_description(
                "Description.", {10}, set(), ["other/repo#1000", "other/repo#999", "a/b#5"]
            ),
            "Description.\n\nLand after #10.\nLand after a/b#5.\n"
            "Land after other/repo#999.\nLand after other/repo#1000.",
        )

    def test_retains_external_land_with_and_land_before(self):
        """External land-with and land-before lines are retained."""
        body = "Land before other/repo#3.\nLand with other/repo#2.\nLand after other/repo#1."
        self.assertEqual(
            pr_status.updated_pr_description(body, set(), set(), ["other/repo#2"]),
            "Land with other/repo#2.\nLand before other/repo#3.",
        )


class StatedPrerequisitesTest(unittest.TestCase):
    """Tests for stated_prerequisites."""

    repo = types.SimpleNamespace(gh="owner/repo", keywords=[])

    def test_land_with_is_partner(self):
        """A land-with PR is a partner, not a prerequisite."""
        self.assertEqual(
            pr_status.stated_prerequisites("Land with #5.", self.repo),
            (set(), set(), set(), {5}, set()),
        )

    def test_after_with_before(self):
        """One sentence can name a prerequisite, a partner, and a dependent."""
        self.assertEqual(
            pr_status.stated_prerequisites("Land it after #1, with #2, and before #3.", self.repo),
            ({1}, set(), {3}, {2}, set()),
        )

    def test_land_with_other_repository(self):
        """A land-with PR in another repository is an external partner."""
        self.assertEqual(
            pr_status.stated_prerequisites("Land with other/repo#5.", self.repo),
            (set(), set(), set(), set(), {"other/repo#5"}),
        )


class ModelTest(unittest.TestCase):
    """Tests for Model."""

    @staticmethod
    def raw_model(bodies, contains, gh="owner/repo"):
        """Build a model, without its graph, of open PRs.

        The PRs have the given descriptions and branch containment.

        Returns:
            The model.
        """
        prs = {
            n: {"body": body, "baseRefName": "master", "owner": "owner", "headRefName": f"b{n}"}
            for n, body in bodies.items()
        }
        repo = types.SimpleNamespace(
            name=gh,
            gh=gh,
            owner=gh.split("/")[0],
            default_branch="master",
            keywords=[],
            prs=prs,
            contains=contains,
            closed_state={},
            state_of=lambda _: None,
        )
        return pr_status.Model(repo, {"repos": {gh: {}}})

    def model(self, bodies, contains):
        """Build a model of open PRs with the given descriptions and branch containment.

        Returns:
            The model.
        """
        model = self.raw_model(bodies, contains)
        pr_status.unite_partners([model])
        return model

    def test_containment_is_dependency(self):
        """A PR whose branch contains another's commits depends on it."""
        model = self.model({1: "", 2: ""}, {(1, 2)})
        self.assertEqual(model.blocked(), [1])

    def test_land_with_containment_is_not_dependency(self):
        """A PR does not depend on its land-with partner, even if it contains its commits."""
        for bodies in ({1: "Land with #2.", 2: ""}, {1: "", 2: "Land with #1."}):
            model = self.model(bodies, {(1, 2)})
            self.assertEqual(model.blocked(), [])

    def test_dependency_on_pr_covers_its_land_with_partner(self):
        """A dependency on a land-with partner is not direct if the PR contains the partner."""
        model = self.model({1: "Land with #2.", 2: "", 3: ""}, {(1, 2), (3, 1), (3, 2)})
        self.assertEqual(model.direct[3], {1})
        self.assertEqual(model.all_deps[3], {1, 2})

    def test_unit_members_share_prerequisites(self):
        """Each member of a unit depends on the prerequisites of every member."""
        model = self.model({1: "Land with #2.", 2: "Land with #3.", 3: "", 4: ""}, {(3, 4)})
        self.assertEqual(model.blocked(), [1, 2, 3])
        self.assertEqual(model.direct, {1: {4}, 2: {4}, 3: {4}, 4: set()})

    def test_unit_spans_repositories(self):
        """A unit's members in different repositories share their prerequisites."""
        a = self.raw_model({1: "Land with other/repo#5.", 2: ""}, set())
        b = self.raw_model(
            {5: "Land after #6.", 6: "", 7: "Land with owner/repo#2."}, set(), "other/repo"
        )
        pr_status.unite_partners([a, b])
        self.assertEqual(a.blocked(), [1])
        self.assertEqual(a.external[1], ["other/repo#6"])
        self.assertEqual(b.blocked(), [5])
        self.assertEqual(b.external[7], [])


class ApplyDescriptionUpdatesTest(unittest.TestCase):
    """Tests for apply_description_updates."""

    def test_failed_update_does_not_change_local_description(self):
        """A rejected GitHub update is returned and leaves the fetched body intact."""
        repo = types.SimpleNamespace(
            gh="owner/repo", prs={1: {"body": "old"}}, ref=lambda n: f"#{n}"
        )
        model = types.SimpleNamespace(repo=repo)
        error = pr_status.subprocess.CalledProcessError(1, ["gh"], stderr="denied")
        with (
            mock.patch.object(pr_status, "run", side_effect=error),
            mock.patch.object(pr_status, "refresh_description_facts") as refresh,
        ):
            failures = pr_status.apply_description_updates([(model, 1, "new")], {})
        self.assertEqual(failures, [(model, 1, error)])
        self.assertEqual(repo.prs[1]["body"], "old")
        refresh.assert_not_called()


class ChainHasConflictTest(unittest.TestCase):
    """Tests for chain_has_conflict."""

    def test_clean_chain(self):
        """A chain with no reported conflict is clean."""
        repo = types.SimpleNamespace(master_conflicts={}, pair_conflicts={})
        self.assertFalse(pr_status.chain_has_conflict(repo, [1, 2, 3], True))

    def test_master_conflict_anywhere_in_chain(self):
        """An upstream conflict in a later PR affects the merge that reaches that PR."""
        repo = types.SimpleNamespace(master_conflicts={2: ["file"]}, pair_conflicts={})
        self.assertTrue(pr_status.chain_has_conflict(repo, [1, 2, 3], True))
        self.assertFalse(pr_status.chain_has_conflict(repo, [1, 2, 3], False))

    def test_nonadjacent_pr_conflict(self):
        """A later merge receives earlier, nonadjacent PRs in the chain."""
        repo = types.SimpleNamespace(master_conflicts={}, pair_conflicts={(1, 3): ["file"]})
        self.assertTrue(pr_status.chain_has_conflict(repo, [1, 2, 3], True))


class PushChainsTest(unittest.TestCase):
    """Tests for push_chains."""

    @staticmethod
    def chains(direct, master_conflicts, has_upstream=True, conflicts_only=False):
        """Run push_chains on PRs whose direct prerequisites are direct.

        Each PR n has the working copy "pr<n>".

        Returns:
            The chains.
        """
        repo = types.SimpleNamespace(
            gh="owner/repo",
            default_branch="master",
            prs={n: {} for n in direct},
            master_conflicts=master_conflicts,
            pair_conflicts={},
        )
        levels = {n: 1 + len(ds) for n, ds in direct.items() if ds} | {
            d: 1 for ds in direct.values() for d in ds
        }
        model = types.SimpleNamespace(
            repo=repo, open=set(direct), direct=direct, levels=lambda: levels
        )
        upstream = {"master": ("git", "owner", "repo", "master")} if has_upstream else {}
        # pr_directories receives the PR's data, so look up the PR's number by identity.
        ids = {id(repo.prs[n]): n for n in direct}
        with (
            mock.patch.object(pr_status, "scan_directories", return_value=upstream),
            mock.patch.object(
                pr_status, "pr_directories", lambda _r, pr, _base: [f"pr{ids[id(pr)]}"]
            ),
        ):
            return pr_status.push_chains(model, "/base", conflicts_only)[0]

    def test_single_pr_chain(self):
        """A PR with no prerequisite or dependent forms a chain with upstream."""
        chains = self.chains({1: set(), 2: set(), 3: {2}}, {})
        self.assertEqual(chains, [["master", "pr2", "pr3"], ["master", "pr1"]])

    def test_single_pr_chain_conflicts_only(self):
        """A single-PR chain is retained exactly when its PR conflicts with upstream."""
        chains = self.chains({1: set(), 2: set(), 3: {2}}, {1: ["file"]}, conflicts_only=True)
        self.assertEqual(chains, [["master", "pr1"]])

    def test_no_single_pr_chain_without_upstream(self):
        """Without upstream, a single PR has nothing to push into it."""
        chains = self.chains({1: set(), 2: set(), 3: {2}}, {}, has_upstream=False)
        self.assertEqual(chains, [["pr2", "pr3"]])


class SectionGeneratorsTest(unittest.TestCase):
    """Tests for section_generators."""

    def test_generates_conflicting_commands_section(self):
        """The conflict-only section is generated without any configuration."""
        generators = pr_status.section_generators([], {"documents": {}}, "/base")
        self.assertIn("conflicting-push-commands", generators)


if __name__ == "__main__":
    unittest.main()
