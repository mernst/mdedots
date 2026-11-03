"""Tests for mail-delete-closed-prs.py that do not access Gmail, GitHub, or OAuth credentials."""

# The fake Google API objects intentionally implement only their public test surface.
# ruff: file-ignore[undocumented-public-method, undocumented-public-init, docstring-missing-returns]

from __future__ import annotations

import sys
import unittest
from pathlib import Path
from unittest import mock

SCRIPT_DIRECTORY = Path(__file__).parent
if str(SCRIPT_DIRECTORY) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIRECTORY))

from script_loader import load_script  # ruff: ignore[module-import-not-at-top-of-file]

mdcp = load_script("mail-delete-closed-prs.py")

GITHUB, AZURE, CIRCLECI = mdcp.SOURCES
IssueRef = mdcp.IssueRef
BranchRef = mdcp.BranchRef
RefState = mdcp.RefState
Disposition = mdcp.Disposition


def thread(subjects, senders=None, starred=False):
    """Return a thread summary whose messages have the given subjects."""
    if senders is None:
        senders = ("notifications@github.com",) * len(subjects)
    return mdcp.ThreadSummary("t1", tuple(subjects), tuple(senders), starred)


def pr(number, state, owner):
    """Return a pull request as the GitHub GraphQL API describes it in ref_query_field."""
    return {"number": number, "state": state, "headRepositoryOwner": {"login": owner}}


def repository(pull_requests=(), parent=None, default_branch="master"):
    """Return a repository as the GitHub GraphQL API describes it for a BranchRef."""
    return {
        "defaultBranchRef": {"name": default_branch},
        "pullRequests": {"nodes": list(pull_requests)},
        "parent": parent,
    }


def parent_repository(slug, pull_requests=()):
    """Return a parent repository as the GitHub GraphQL API describes it for a BranchRef."""
    return {"nameWithOwner": slug, "pullRequests": {"nodes": list(pull_requests)}}


class ParseSubjectTest(unittest.TestCase):
    """Test the parsing of notification subjects."""

    def test_github_pull_request(self):
        self.assertEqual(
            mdcp.parse_github_subject("[codespecs/daikon] spinfo: match names (PR #926)"),
            IssueRef("codespecs/daikon", 926),
        )

    def test_github_issue_reply(self):
        self.assertEqual(
            mdcp.parse_github_subject("Re: [plume-lib/plume-util] A (nested) bug (Issue #12)"),
            IssueRef("plume-lib/plume-util", 12),
        )

    def test_github_old_format(self):
        self.assertEqual(
            mdcp.parse_github_subject("[randoop/randoop] Create API (#943)"),
            IssueRef("randoop/randoop", 943),
        )

    def test_github_other_notification(self):
        self.assertIsNone(mdcp.parse_github_subject("[codespecs/daikon] Run failed: CI - main"))
        self.assertIsNone(mdcp.parse_github_subject("[codespecs/daikon] Mention PR #926 here"))
        self.assertIsNone(
            mdcp.parse_github_subject("[md-reader/md-reader] Feedback (Discussion #51)")
        )
        self.assertIsNone(mdcp.parse_github_subject("[mernst/daikon] Run failed: CI - x (84af65d)"))

    def test_azure(self):
        self.assertEqual(
            mdcp.parse_azure_subject(
                "[Build failed] mernst.daikon - mernst/daikon:splitter-quant-calls"
                " - daikon - 5429bfbb"
            ),
            BranchRef("mernst/daikon", "splitter-quant-calls"),
        )
        self.assertEqual(
            mdcp.parse_azure_subject(
                "[Build succeeded] mernst.fjalar - mernst/fjalar:dependabot/x-1.2 - daikon - b79"
            ),
            BranchRef("mernst/fjalar", "dependabot/x-1.2"),
        )
        self.assertIsNone(mdcp.parse_azure_subject("Azure DevOps personal access token added"))

    def test_circleci(self):
        self.assertEqual(
            mdcp.parse_circleci_subject(
                "[CircleCI] Workflow failed: mernst / daikon on nonce-fixer-pairing"
            ),
            BranchRef("mernst/daikon", "nonce-fixer-pairing"),
        )
        self.assertIsNone(mdcp.parse_circleci_subject("Your AI software factory | Commit #19"))


class ThreadSummaryTest(unittest.TestCase):
    """Test the summarizing of Gmail threads."""

    def test_from_gmail_thread(self):
        gmail_thread = {
            "id": "abc",
            "messages": [
                {
                    "labelIds": ["INBOX"],
                    "payload": {
                        "headers": [
                            {"name": "Subject", "value": "[o/r] Title\r\n (PR #1)"},
                            {"name": "From", "value": "A Person <notifications@github.com>"},
                        ]
                    },
                },
                {
                    "labelIds": ["STARRED"],
                    "payload": {"headers": [{"name": "From", "value": "Me@Example.com"}]},
                },
            ],
        }
        self.assertEqual(
            mdcp.ThreadSummary.from_gmail_thread(gmail_thread),
            mdcp.ThreadSummary(
                "abc",
                ("[o/r] Title (PR #1)", ""),
                ("notifications@github.com", "me@example.com"),
                True,
            ),
        )


class ThreadVerdictTest(unittest.TestCase):
    """Test the decision whether to trash a thread."""

    REF = IssueRef("o/r", 1)
    OTHER_REF = IssueRef("o/r", 2)
    SUBJECT = "[o/r] Title (PR #1)"
    OTHER_SUBJECT = "[o/r] Other (PR #2)"
    CLOSED = RefState(True, "PR o/r#1 merged")
    OPEN = RefState(False, "PR o/r#2 open")

    def verdict(self, summary, states):
        return mdcp.thread_verdict(summary, GITHUB, states).disposition

    def test_closed(self):
        self.assertIs(
            self.verdict(thread([self.SUBJECT]), {self.REF: self.CLOSED}), Disposition.TRASH
        )

    def test_open(self):
        self.assertIs(
            self.verdict(thread([self.SUBJECT]), {self.REF: self.OPEN}), Disposition.NOT_CLOSED
        )

    def test_every_ref_must_be_closed(self):
        summary = thread([self.SUBJECT, self.OTHER_SUBJECT])
        states = {self.REF: self.CLOSED, self.OTHER_REF: self.OPEN}
        self.assertIs(self.verdict(summary, states), Disposition.NOT_CLOSED)

    def test_unknown(self):
        unknown = RefState(None, "GitHub did not report o/r")
        self.assertIs(
            self.verdict(thread([self.SUBJECT]), {self.REF: unknown}), Disposition.UNKNOWN
        )
        self.assertIs(self.verdict(thread([self.SUBJECT]), {}), Disposition.UNKNOWN)

    def test_starred(self):
        summary = thread([self.SUBJECT], starred=True)
        self.assertIs(self.verdict(summary, {self.REF: self.CLOSED}), Disposition.STARRED)

    def test_other_sender(self):
        summary = thread(
            [self.SUBJECT, "Re: " + self.SUBJECT],
            senders=("notifications@github.com", "me@example.com"),
        )
        self.assertIs(self.verdict(summary, {self.REF: self.CLOSED}), Disposition.OTHER_SENDER)

    def test_unparsed(self):
        summary = thread([self.SUBJECT, "[o/r] Run failed: CI - main"])
        self.assertIs(self.verdict(summary, {self.REF: self.CLOSED}), Disposition.UNPARSED)


class RefStateTest(unittest.TestCase):
    """Test the interpretation of GitHub's answers."""

    BRANCH = BranchRef("mernst/daikon", "my-branch")

    def test_issue_and_pull_request(self):
        ref = IssueRef("o/r", 5)

        def state(typename, value):
            return mdcp.ref_state(
                ref, {"issueOrPullRequest": {"__typename": typename, "state": value}}
            )

        self.assertEqual(state("PullRequest", "MERGED"), RefState(True, "PR o/r#5 merged"))
        self.assertEqual(state("PullRequest", "CLOSED"), RefState(True, "PR o/r#5 closed"))
        self.assertEqual(state("Issue", "CLOSED"), RefState(True, "issue o/r#5 closed"))
        self.assertFalse(state("Issue", "OPEN").closed)
        self.assertIsNone(mdcp.ref_state(ref, {"issueOrPullRequest": None}).closed)
        self.assertIsNone(mdcp.ref_state(ref, None).closed)

    def test_default_branch(self):
        ref = BranchRef("mernst/fjalar", "master")
        merged = repository([pr(1, "MERGED", "mernst")])
        self.assertFalse(mdcp.ref_state(ref, merged).closed)

    def test_branch_without_pull_request(self):
        self.assertEqual(
            mdcp.ref_state(self.BRANCH, repository()),
            RefState(False, "no PR from mernst/daikon:my-branch"),
        )

    def test_branch_with_merged_pull_request_to_parent(self):
        parent = parent_repository("codespecs/daikon", [pr(918, "MERGED", "mernst")])
        self.assertEqual(
            mdcp.ref_state(self.BRANCH, repository(parent=parent)),
            RefState(True, "PR from mernst/daikon:my-branch: codespecs/daikon#918 merged"),
        )

    def test_branch_with_open_pull_request(self):
        parent = parent_repository(
            "codespecs/daikon", [pr(918, "CLOSED", "mernst"), pr(964, "OPEN", "mernst")]
        )
        self.assertFalse(mdcp.ref_state(self.BRANCH, repository(parent=parent)).closed)

    def test_other_owners_branch_is_ignored(self):
        # A pull request from a deleted fork has no head repository owner.
        from_deleted_fork = {"number": 901, "state": "MERGED", "headRepositoryOwner": None}
        parent = parent_repository(
            "codespecs/daikon", [pr(900, "MERGED", "someone"), from_deleted_fork]
        )
        self.assertEqual(
            mdcp.ref_state(self.BRANCH, repository(parent=parent)),
            RefState(False, "no PR from mernst/daikon:my-branch"),
        )

    def test_same_repository_pull_request(self):
        ref = BranchRef("codespecs/daikon", "feature")
        self.assertTrue(mdcp.ref_state(ref, repository([pr(7, "CLOSED", "codespecs")])).closed)

    def test_unknown_repository(self):
        self.assertIsNone(mdcp.ref_state(self.BRANCH, None).closed)


class RefStatesTest(unittest.TestCase):
    """Test the batching of GitHub queries."""

    def test_batches_and_aliases(self):
        refs = [IssueRef("o/r", number) for number in range(3)] + [IssueRef("o/r", 0)]
        queries = []

        def run_graphql(query):
            queries.append(query)
            count = query.count("issueOrPullRequest")
            return {
                f"r{index}": {"issueOrPullRequest": {"__typename": "Issue", "state": "CLOSED"}}
                for index in range(count)
            }

        with mock.patch.object(mdcp, "GITHUB_BATCH_SIZE", 2):
            states = mdcp.ref_states(refs, run_graphql)
        self.assertEqual(len(queries), 2)
        self.assertIn('r0: repository(owner: "o", name: "r")', queries[0])
        self.assertIn("issueOrPullRequest(number: 1)", queries[0])
        self.assertEqual(set(states), set(refs))
        self.assertTrue(all(state.closed for state in states.values()))

    def test_branch_query_escapes_names(self):
        field = mdcp.ref_query_field("r0", BranchRef("o/r", 'a"b'))
        self.assertIn(r'headRefName: "a\"b"', field)
        self.assertIn("parent { nameWithOwner pullRequests", field)


class FakeBatch:
    """A fake Gmail batch request whose requests succeed or fail as the test specifies."""

    def __init__(self, callback, outcomes):
        self.callback = callback
        self.outcomes = outcomes
        self.request_ids = []

    def add(self, _request, request_id):
        self.request_ids.append(request_id)

    def execute(self):
        for request_id in self.request_ids:
            outcome = self.outcomes[request_id].pop(0)
            if isinstance(outcome, Exception):
                self.callback(request_id, None, outcome)
            else:
                self.callback(request_id, outcome, None)


class FakeBatchService:
    """A fake Gmail service that creates FakeBatch requests."""

    def __init__(self, outcomes):
        self.outcomes = outcomes

    def new_batch_http_request(self, callback):
        return FakeBatch(callback, self.outcomes)


def http_error(status, message="error"):
    """Return an exception shaped like googleapiclient.errors.HttpError."""
    return type(
        "FakeHttpError", (Exception,), {"resp": type("Response", (), {"status": status})()}
    )(message)


class BatchExecuteTest(unittest.TestCase):
    """Test the retrying of Gmail batch requests."""

    def requests(self, outcomes):
        return {request_id: lambda: None for request_id in outcomes}

    def test_transient_failure_is_retried(self):
        outcomes = {"a": [http_error(429), "A"], "b": ["B"]}
        sleeps = []
        responses = mdcp.batch_execute(
            FakeBatchService(outcomes), self.requests(outcomes), "fetch thread", sleeps.append
        )
        self.assertEqual(responses, {"a": "A", "b": "B"})
        self.assertEqual(sleeps, [1])

    def test_permanent_failure_is_raised(self):
        outcomes = {"a": [http_error(404, "not found")]}
        with self.assertRaisesRegex(RuntimeError, "cannot fetch thread a: not found"):
            mdcp.batch_execute(FakeBatchService(outcomes), self.requests(outcomes), "fetch thread")

    def test_repeated_transient_failure_is_raised(self):
        outcomes = {"a": [http_error(503)] * (mdcp.GMAIL_RETRIES + 1)}
        sleeps = []
        with self.assertRaisesRegex(RuntimeError, "cannot trash thread a: error"):
            mdcp.batch_execute(
                FakeBatchService(outcomes), self.requests(outcomes), "trash thread", sleeps.append
            )
        self.assertEqual(sleeps, [2**attempt for attempt in range(mdcp.GMAIL_RETRIES)])


if __name__ == "__main__":
    unittest.main()
