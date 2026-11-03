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
    message_ids = tuple(f"m{index}" for index in range(len(subjects)))
    return mdcp.ThreadSummary("t1", message_ids, tuple(subjects), tuple(senders), starred)


def pr(number, state, owner):
    """Return a pull request, as a dict like one parsed from GitHub's JSON for PR_FIELDS.

    The dict has the keys "number", "state", and "headRepositoryOwner".
    """
    return {"number": number, "state": state, "headRepositoryOwner": {"login": owner}}


def pull_request_connection(pull_requests, has_next_page):
    """Return a dict like one parsed from GitHub's JSON for a pullRequests field.

    The dict has the keys "nodes" and "pageInfo".
    """
    return {"nodes": list(pull_requests), "pageInfo": {"hasNextPage": has_next_page}}


def repository(pull_requests=(), parent=None, default_branch="master", has_next_page=False):
    """Return a repository, as a dict like one parsed from GitHub's JSON for a BranchRef.

    The dict has the keys "defaultBranchRef", "pullRequests", and "parent".
    """
    return {
        "defaultBranchRef": {"name": default_branch},
        "pullRequests": pull_request_connection(pull_requests, has_next_page),
        "parent": parent,
    }


def parent_repository(slug, pull_requests=(), has_next_page=False):
    """Return a parent repository, as a dict like one parsed from GitHub's JSON for a BranchRef.

    The dict has the keys "nameWithOwner" and "pullRequests".
    """
    return {
        "nameWithOwner": slug,
        "pullRequests": pull_request_connection(pull_requests, has_next_page),
    }


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


class DefaultWindowTest(unittest.TestCase):
    """Test the default search window."""

    def test_no_terms(self):
        self.assertEqual(mdcp.with_default_window(""), "newer_than:31d")

    def test_terms_without_date(self):
        self.assertEqual(mdcp.with_default_window("in:inbox"), "in:inbox newer_than:31d")

    def test_terms_with_date(self):
        for query in (
            "newer_than:7d",
            "in:inbox older_than:1y",
            "after:2000/01/01",
            "-before:2026/01/01",
            "(NEWER:2026/10/01)",
        ):
            with self.subTest(query=query):
                self.assertEqual(mdcp.with_default_window(query), query)


class ThreadSummaryTest(unittest.TestCase):
    """Test the summarizing of Gmail threads."""

    def test_from_gmail_thread(self):
        gmail_thread = {
            "id": "abc",
            "messages": [
                {
                    "id": "m1",
                    "labelIds": ["INBOX"],
                    "payload": {
                        "headers": [
                            {"name": "Subject", "value": "[o/r] Title\r\n (PR #1)"},
                            {"name": "From", "value": "A Person <notifications@github.com>"},
                        ]
                    },
                },
                {
                    "id": "m2",
                    "labelIds": ["STARRED"],
                    "payload": {"headers": [{"name": "From", "value": "Me@Example.com"}]},
                },
            ],
        }
        self.assertEqual(
            mdcp.ThreadSummary.from_gmail_thread(gmail_thread),
            mdcp.ThreadSummary(
                "abc",
                ("m1", "m2"),
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

    def test_too_many_pull_requests(self):
        parent = parent_repository(
            "codespecs/daikon", [pr(918, "MERGED", "mernst")], has_next_page=True
        )
        self.assertIsNone(mdcp.ref_state(self.BRANCH, repository(parent=parent)).closed)
        self.assertIsNone(mdcp.ref_state(self.BRANCH, repository(has_next_page=True)).closed)

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
        self.assertIn("pageInfo { hasNextPage }", field)


class FakeBatch:
    """A fake Gmail batch request whose requests succeed or fail as the test specifies."""

    def __init__(self, callback, outcomes, batch_errors):
        self.callback = callback
        self.outcomes = outcomes
        self.batch_errors = batch_errors
        self.request_ids = []

    def add(self, _request, request_id):
        self.request_ids.append(request_id)

    def execute(self):
        if self.batch_errors:
            raise self.batch_errors.pop(0)
        for request_id in self.request_ids:
            outcome = self.outcomes[request_id].pop(0)
            if isinstance(outcome, Exception):
                self.callback(request_id, None, outcome)
            else:
                self.callback(request_id, outcome, None)


class FakeBatchService:
    """A fake Gmail service that creates FakeBatch requests.

    `batch_errors` holds exceptions that the next batches raise as a whole.
    """

    def __init__(self, outcomes, batch_errors=()):
        self.outcomes = outcomes
        self.batch_errors = list(batch_errors)
        self.batches = []

    def new_batch_http_request(self, callback):
        batch = FakeBatch(callback, self.outcomes, self.batch_errors)
        self.batches.append(batch.request_ids)
        return batch


def http_error(status, message="error"):
    """Return an exception shaped like googleapiclient.errors.HttpError."""
    return type(
        "FakeHttpError", (Exception,), {"resp": type("Response", (), {"status": status})()}
    )(message)


class BatchExecuteTest(unittest.TestCase):
    """Test the retrying and pacing of Gmail batch requests."""

    UNITS = 10

    def requests(self, outcomes):
        return {request_id: lambda: None for request_id in outcomes}

    def execute(self, service, outcomes, sleeps):
        return mdcp.batch_execute(service, self.requests(outcomes), self.UNITS, sleeps.append)

    def test_transient_failure_is_retried(self):
        outcomes = {"a": [http_error(429), "A"], "b": ["B"]}
        sleeps = []
        result = self.execute(FakeBatchService(outcomes), outcomes, sleeps)
        self.assertEqual(result, mdcp.BatchResult({"a": "A", "b": "B"}, {}))
        self.assertEqual(sleeps, [1])

    def test_permanent_failure_is_reported(self):
        not_found = http_error(404, "not found")
        outcomes = {"a": [not_found], "b": [http_error(503), "B"]}
        result = self.execute(FakeBatchService(outcomes), outcomes, [])
        self.assertEqual(result, mdcp.BatchResult({"b": "B"}, {"a": not_found}))

    def test_repeated_transient_failure_is_reported(self):
        unavailable = http_error(503)
        outcomes = {"a": [unavailable] * (mdcp.GMAIL_RETRIES + 1), "b": ["B"]}
        sleeps = []
        result = self.execute(FakeBatchService(outcomes), outcomes, sleeps)
        self.assertEqual(result, mdcp.BatchResult({"b": "B"}, {"a": unavailable}))
        self.assertEqual(sleeps, [2**attempt for attempt in range(mdcp.GMAIL_RETRIES)])

    def test_failure_of_whole_batch_is_retried(self):
        outcomes = {"a": ["A"], "b": ["B"]}
        service = FakeBatchService(outcomes, [TimeoutError("timed out")])
        result = self.execute(service, outcomes, [])
        self.assertEqual(result, mdcp.BatchResult({"a": "A", "b": "B"}, {}))

    def test_batches_are_paced(self):
        outcomes = {str(index): ["R"] for index in range(mdcp.GMAIL_BATCH_SIZE * 2 + 1)}
        sleeps = []
        service = FakeBatchService(outcomes)
        result = self.execute(service, outcomes, sleeps)
        self.assertEqual(len(result.responses), len(outcomes))
        self.assertEqual(
            [len(request_ids) for request_ids in service.batches],
            [mdcp.GMAIL_BATCH_SIZE, mdcp.GMAIL_BATCH_SIZE, 1],
        )
        interval = mdcp.GMAIL_BATCH_SIZE * self.UNITS / mdcp.GMAIL_QUOTA_UNITS_PER_SECOND
        self.assertEqual(sleeps, [interval, interval])


if __name__ == "__main__":
    unittest.main()
