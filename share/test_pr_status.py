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
        )
        self.assertEqual(pr_status.description_updates([model]), [])


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


if __name__ == "__main__":
    unittest.main()
