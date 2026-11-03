#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "google-api-python-client",
#   "google-auth",
#   "google-auth-oauthlib",
# ]
# ///

"""Move Gmail notifications about closed pull requests and issues to the trash.

Usage: mail-delete-closed-prs.py [--delete] [--query GMAIL-SEARCH] [--account EMAIL]

The script examines notifications from three services:
 * GitHub, whose subjects name a pull request or issue, as in
   "[codespecs/daikon] Fix a bug (PR #926)".  Such a notification is about a
   closed pull request or issue if it is closed or merged.
 * Azure DevOps, whose subjects name a branch, as in
   "[Build failed] mernst.daikon - mernst/daikon:my-branch - daikon - 5429bfbb".
 * CircleCI, whose subjects name a branch, as in
   "[CircleCI] Workflow failed: mernst / daikon on my-branch".
A notification about a branch is about a closed pull request if at least one
pull request was made from that branch, to its repository or to the
repository's parent, and none of those pull requests is open.  A notification
about a repository's default branch is never trashed.

The script keeps a thread if any of its messages is starred, if any message
comes from a sender other than the service's notification address (such as a
reply that you sent), or if it cannot determine the state of the pull request,
issue, or branch.  It does not examine mail in the spam or trash folders.

By default, the script only reports what it would trash; --delete trashes it.
The script trashes only the messages that it examined, so a message that
arrives in a thread while the script runs stays out of the trash.
--query adds Gmail search terms, such as "in:inbox", to every search.  If
those terms restrict no date, the script searches only the last 31 days; to
search all mail, give a date such as "after:2000/01/01".

--account selects the Gmail account to examine, by its email address; it is
one of the accounts in mail_review.ACCOUNT_CONFIGS, and the default is the
first of them.

If any Gmail request fails, the script exits with a nonzero status after
doing the rest of its work.

The script uses the `gh` command to query GitHub.  See mail_review.py, in this
directory, for the OAuth client-secret file that it needs.  On the first run
for an account, the script asks the user to authorize it to modify mail; it
caches that authorization in the file that modify_account names.
"""

from __future__ import annotations

import argparse
import dataclasses
import enum
import json
import re
import subprocess
import time
from collections import Counter
from dataclasses import dataclass
from email.utils import parseaddr
from typing import TYPE_CHECKING, Any

from mail_review import (
    ACCOUNT_CONFIGS,
    CLIENT_SECRET_PATH,
    GMAIL_MODIFY_SCOPE,
    AccountConfig,
    gmail_service,
    is_transient_api_error,
    warn,
)

if TYPE_CHECKING:
    from collections.abc import Callable, Iterable, Mapping

# The Gmail search term that restricts every search if --query restricts no date.
DEFAULT_WINDOW = "newer_than:31d"
# Matches a Gmail search operator that restricts dates, such as the "newer_than:"
# of "newer_than:7d" or the "-after:" of "-after:2026/01/01".
DATE_OPERATOR = re.compile(
    r"(?<![\w:])-?(?:after|before|newer|older|newer_than|older_than):", re.IGNORECASE
)
# The number of Gmail requests per batch.
GMAIL_BATCH_SIZE = 20
# The number of Gmail quota units that a user can spend per second.
GMAIL_QUOTA_UNITS_PER_SECOND = 250
# The number of Gmail quota units that a threads.get request costs.
THREADS_GET_UNITS = 10
# The number of Gmail quota units that a messages.trash request costs.
MESSAGES_TRASH_UNITS = 5
# The number of times to retry Gmail requests that fail transiently.
GMAIL_RETRIES = 6
# The number of repository lookups in each GitHub GraphQL query.
GITHUB_BATCH_SIZE = 50
# The maximum number of pull requests per branch and repository that one
# GitHub query reports.  A branch with more is treated as having an unknown state.
MAX_PRS_PER_BRANCH = 100


@dataclass(frozen=True)
class IssueRef:
    """A GitHub issue or pull request, such as codespecs/daikon#926.

    `slug` is the repository's GitHub slug, such as "codespecs/daikon".
    """

    slug: str
    number: int

    def __str__(self) -> str:
        """Return the ref in GitHub's notation.

        Returns:
            The ref, as in "codespecs/daikon#926".
        """
        return f"{self.slug}#{self.number}"


@dataclass(frozen=True)
class BranchRef:
    """A branch of a GitHub repository, such as mernst/daikon:my-branch.

    `slug` is the repository's GitHub slug, such as "mernst/daikon".
    """

    slug: str
    branch: str

    def __str__(self) -> str:
        """Return the ref in GitHub's notation.

        Returns:
            The ref, as in "mernst/daikon:my-branch".
        """
        return f"{self.slug}:{self.branch}"


Ref = IssueRef | BranchRef


@dataclass(frozen=True)
class RefState:
    """What GitHub reports about a ref.

    `closed` is True if the ref is about a closed pull request or issue, False
    if it is not, and None if GitHub did not report its state.  `description`
    explains the state to the user, as in "codespecs/daikon#918 merged".
    """

    closed: bool | None
    description: str


@dataclass(frozen=True)
class Source:
    """A service that sends notifications about pull requests, issues, or branches.

    `sender` is the address from which the service sends notifications,
    `subject_filter` holds Gmail search terms, such as "subject:Build", that
    restrict the sender's mail to notifications, and `parse` returns the ref that
    a notification's subject names, or None if the subject names none.
    """

    name: str
    sender: str
    subject_filter: str
    parse: Callable[[str], Ref | None]

    @property
    def query(self) -> str:
        """The Gmail search that finds this service's notifications.

        An example is "from:azuredevops@microsoft.com subject:Build".
        """
        return f"from:{self.sender} {self.subject_filter}".strip()


@dataclass(frozen=True)
class ThreadSummary:
    """The parts of a Gmail thread that determine whether to trash it.

    `message_ids`, `subjects`, and `senders` have one entry per message.  A
    message id is a hexadecimal string such as "19a3c2f1b07d8e45".  Each sender is an
    email address, such as "notifications@github.com", without a display name.
    `starred` is True if any message in the thread is starred.
    """

    thread_id: str
    message_ids: tuple[str, ...]
    subjects: tuple[str, ...]
    senders: tuple[str, ...]
    starred: bool

    @classmethod
    def from_gmail_thread(cls, thread: Mapping[str, Any]) -> ThreadSummary:
        """Return the summary of the given thread.

        Args:
            thread: a thread, as a dict parsed from the Gmail API's JSON for a
                threads.get request in "metadata" format, with keys such as "id"
                and "messages"

        Returns:
            The summary of the thread.
        """
        message_ids = []
        subjects = []
        senders = []
        starred = False
        for message in thread.get("messages", ()):
            message_ids.append(message["id"])
            headers = {
                header["name"].casefold(): header["value"]
                for header in message.get("payload", {}).get("headers", ())
            }
            subjects.append(" ".join(headers.get("subject", "").split()))
            senders.append(parseaddr(headers.get("from", ""))[1].casefold())
            starred = starred or "STARRED" in message.get("labelIds", ())
        return cls(thread["id"], tuple(message_ids), tuple(subjects), tuple(senders), starred)


# Matches a GitHub notification's subject, such as
# "Re: [codespecs/daikon] Fix a bug (PR #926)", capturing the slug and number.
# The parenthesized part is "(PR #926)", "(Issue #926)", or, in a thread that
# GitHub started before it used those forms, "(#926)".
GITHUB_SUBJECT = re.compile(
    r"(?:Re: )?\[(?P<slug>[\w.-]+/[\w.-]+)\] .*\((?:PR |Issue )?#(?P<number>\d+)\)"
)
# Matches an Azure DevOps build notification's subject, such as
# "[Build failed] mernst.daikon - mernst/daikon:my-branch - daikon - 5429bfbb",
# capturing the slug and branch.
AZURE_SUBJECT = re.compile(r"\[Build [^\]]+\] .*? - (?P<slug>[\w.-]+/[\w.-]+):(?P<branch>\S+) - .*")
# Matches a CircleCI workflow notification's subject, such as
# "[CircleCI] Workflow failed: mernst / daikon on my-branch", capturing the
# owner, repository name, and branch.
CIRCLECI_SUBJECT = re.compile(
    r"\[CircleCI\] Workflow [^:]+: (?P<owner>[\w.-]+) / (?P<repo>[\w.-]+) on (?P<branch>\S+)"
)


def parse_github_subject(subject: str) -> Ref | None:
    """Return the issue or pull request that a GitHub notification's subject names.

    Returns:
        The issue or pull request, or None if the subject names neither.
    """
    match = GITHUB_SUBJECT.fullmatch(subject)
    if match is None:
        return None
    return IssueRef(match["slug"], int(match["number"]))


def parse_azure_subject(subject: str) -> Ref | None:
    """Return the branch that an Azure DevOps build notification's subject names.

    Returns:
        The branch, or None if the subject names none.
    """
    match = AZURE_SUBJECT.fullmatch(subject)
    if match is None:
        return None
    return BranchRef(match["slug"], match["branch"])


def parse_circleci_subject(subject: str) -> Ref | None:
    """Return the branch that a CircleCI workflow notification's subject names.

    Returns:
        The branch, or None if the subject names none.
    """
    match = CIRCLECI_SUBJECT.fullmatch(subject)
    if match is None:
        return None
    return BranchRef(f"{match['owner']}/{match['repo']}", match["branch"])


SOURCES = (
    Source(
        "GitHub",
        "notifications@github.com",
        "",
        parse_github_subject,
    ),
    Source(
        "Azure DevOps",
        "azuredevops@microsoft.com",
        "subject:Build",
        parse_azure_subject,
    ),
    Source(
        "CircleCI",
        "no-reply@builds.circleci.com",
        "subject:Workflow",
        parse_circleci_subject,
    ),
)


class Disposition(enum.Enum):
    """What to do with a thread, and why."""

    TRASH = "closed"
    NOT_CLOSED = "not closed"
    STARRED = "starred"
    OTHER_SENDER = "has a message from another sender"
    UNPARSED = "subject names no pull request, issue, or branch"
    UNKNOWN = "state unknown"


@dataclass(frozen=True)
class Verdict:
    """What to do with a thread, and a description of the refs it is about."""

    disposition: Disposition
    description: str = ""


def thread_refs(thread: ThreadSummary, source: Source) -> tuple[Ref, ...] | None:
    """Return the refs that the given thread's subjects name.

    Args:
        thread: a thread of notifications
        source: the service that sent the thread

    Returns:
        The distinct refs, in order, or None if some subject names no ref.
    """
    refs: list[Ref] = []
    for subject in thread.subjects:
        ref = source.parse(subject)
        if ref is None:
            return None
        if ref not in refs:
            refs.append(ref)
    return tuple(refs) or None


def sender_disposition(thread: ThreadSummary, source: Source) -> Disposition | None:
    """Return why the given thread is kept regardless of its refs' states, or None.

    Args:
        thread: a thread of notifications
        source: the service that sent the thread

    Returns:
        STARRED, OTHER_SENDER, or None if the thread's refs determine whether to trash it.
    """
    if thread.starred:
        return Disposition.STARRED
    if any(sender != source.sender for sender in thread.senders):
        return Disposition.OTHER_SENDER
    return None


def refs_verdict(refs: tuple[Ref, ...] | None, states: Mapping[Ref, RefState]) -> Verdict:
    """Return what to do with a thread whose subjects name the given refs.

    Args:
        refs: a thread's refs, as thread_refs returns them: the refs that its
            subjects name, or None if some subject names none
        states: the state of each of the refs

    Returns:
        A verdict whose disposition is TRASH only if every ref is closed.
    """
    if refs is None:
        return Verdict(Disposition.UNPARSED)
    ref_states = [states.get(ref, RefState(None, f"{ref} not looked up")) for ref in refs]
    description = "; ".join(state.description for state in ref_states)
    if any(state.closed is None for state in ref_states):
        return Verdict(Disposition.UNKNOWN, description)
    if all(state.closed for state in ref_states):
        return Verdict(Disposition.TRASH, description)
    return Verdict(Disposition.NOT_CLOSED, description)


def thread_verdicts(
    found: Iterable[Candidate],
    lookup: Callable[[Iterable[Ref]], Mapping[Ref, RefState]],
) -> list[Verdict]:
    """Return what to do with each of the given threads.

    The routine looks up only the refs of threads that it might trash.

    Args:
        found: a list of notification threads
        lookup: a function that returns the state of each of the refs that it is
            given, as ref_states does

    Returns:
        The verdicts, in the order of the threads.
    """
    found = list(found)
    kept = [sender_disposition(candidate.thread, candidate.source) for candidate in found]
    refs = [
        None if disposition else thread_refs(candidate.thread, candidate.source)
        for candidate, disposition in zip(found, kept, strict=True)
    ]
    states = lookup(ref for candidate_refs in refs for ref in candidate_refs or ())
    return [
        Verdict(disposition) if disposition else refs_verdict(candidate_refs, states)
        for disposition, candidate_refs in zip(kept, refs, strict=True)
    ]


# GitHub queries


PR_FIELDS = "nodes { number state headRepository { nameWithOwner } }"


def _graphql_string(text: str) -> str:
    """Return a GraphQL string literal whose value is the given text.

    Returns:
        The string literal, with its quotation marks.
    """
    # A JSON string literal is also a GraphQL string literal.
    return json.dumps(text)


def _repository_selector(slug: str) -> str:
    """Return a GraphQL field that selects the repository with the given GitHub slug "ORG/REPO".

    Returns:
        The field, without a selection set.
    """
    owner, name = slug.split("/", 1)
    return f"repository(owner: {_graphql_string(owner)}, name: {_graphql_string(name)})"


def ref_query_field(alias: str, ref: Ref) -> str:
    """Return a GraphQL query field, named by the given alias, that reports a ref's state.

    Args:
        alias: a GraphQL alias, such as "r0", which names the field in the response
        ref: the ref whose state to query

    Returns:
        A field of a GraphQL query; ref_state interprets its value.
    """
    if isinstance(ref, IssueRef):
        return (
            f"{alias}: {_repository_selector(ref.slug)} {{"
            f" issueOrPullRequest(number: {ref.number}) {{"
            " __typename ... on Issue { state } ... on PullRequest { state } } }"
        )
    pull_requests = (
        f"pullRequests(headRefName: {_graphql_string(ref.branch)},"
        f" first: {MAX_PRS_PER_BRANCH}) {{ {PR_FIELDS} pageInfo {{ hasNextPage }} }}"
    )
    return (
        f"{alias}: {_repository_selector(ref.slug)} {{"
        " isFork defaultBranchRef { name }"
        f" {pull_requests}"
        f" parent {{ nameWithOwner {pull_requests} }} }}"
    )


def ref_state(ref: Ref, repository: Mapping[str, Any] | None) -> RefState:
    """Return the state of the given ref, given GitHub's answer to ref_query_field.

    Args:
        ref: a ref
        repository: the value of the ref's ref_query_field, as a dict parsed from the
            GitHub GraphQL API's JSON, with keys such as "issueOrPullRequest" or
            "pullRequests"; or None if GitHub did not report it

    Returns:
        The state of the ref.
    """
    if repository is None:
        return RefState(None, f"GitHub did not report {ref.slug}")
    if isinstance(ref, IssueRef):
        item = repository.get("issueOrPullRequest")
        if item is None:
            return RefState(None, f"GitHub did not report {ref}")
        kind = "PR" if item["__typename"] == "PullRequest" else "issue"
        state = item["state"]
        return RefState(state != "OPEN", f"{kind} {ref} {state.lower()}")

    default_branch = (repository.get("defaultBranchRef") or {}).get("name")
    if ref.branch == default_branch:
        return RefState(False, f"{ref} is the default branch")
    head = ref.slug.casefold()
    repositories = [(ref.slug, repository)]
    parent = repository.get("parent")
    if parent is not None:
        repositories.append((parent["nameWithOwner"], parent))
    elif repository.get("isFork") is not False:
        # GitHub reports a null parent for a fork whose parent it does not
        # reveal, as when the parent's organization restricts access.
        return RefState(None, f"GitHub did not report the parent of {ref.slug}")
    pull_requests = []
    for slug, repo in repositories:
        # GitHub reports null for a field that it could not fetch, as when an
        # organization restricts access.
        connection = repo.get("pullRequests")
        if connection is None or None in connection["nodes"]:
            return RefState(None, f"GitHub did not report the PRs in {slug}")
        if connection["pageInfo"]["hasNextPage"]:
            return RefState(
                None,
                f"{slug} has over {MAX_PRS_PER_BRANCH} PRs from branches named {ref.branch}",
            )
        pull_requests.extend(
            (slug, pr)
            for pr in connection["nodes"]
            # A PR from a deleted repository has no head repository.
            if (pr.get("headRepository") or {}).get("nameWithOwner", "").casefold() == head
        )
    if not pull_requests:
        return RefState(False, f"no PR from {ref}")
    description = ", ".join(
        f"{slug}#{pr['number']} {pr['state'].lower()}" for slug, pr in pull_requests
    )
    closed = all(pr["state"] != "OPEN" for _slug, pr in pull_requests)
    return RefState(closed, f"PR from {ref}: {description}")


class GraphQLError(RuntimeError):
    """A GitHub GraphQL query that GitHub did not answer, as when the query timed out."""


def run_gh_graphql(query: str) -> Mapping[str, Any]:
    """Return GitHub's answer to the given GraphQL query, which the `gh` command sends.

    Returns:
        The response's "data" member, as a dict parsed from the GitHub GraphQL
        API's JSON, whose keys are the query's aliases.  A field that GitHub
        did not report, such as a repository that does not exist, is None.

    Raises:
        GraphQLError: if GitHub did not answer the query.
        RuntimeError: if the `gh` command cannot be run.
    """
    try:
        result = subprocess.run(
            ["gh", "api", "graphql", "-f", f"query={query}"],
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError as error:
        msg = f"cannot run gh: {error}"
        raise RuntimeError(msg) from error
    # gh exits with a failure status if any field has an error, such as a
    # repository that does not exist, but it still prints the other fields.
    try:
        response = json.loads(result.stdout)
    except json.JSONDecodeError:
        response = None
    if not isinstance(response, dict) or not isinstance(response.get("data"), dict):
        msg = f"gh api graphql failed: {result.stderr.strip() or result.stdout.strip()}"
        raise GraphQLError(msg)
    return response["data"]


def ref_states(
    refs: Iterable[Ref],
    run_graphql: Callable[[str], Mapping[str, Any]] = run_gh_graphql,
) -> dict[Ref, RefState]:
    """Return the state of each of the given refs.

    Args:
        refs: refs whose states to look up
        run_graphql: runs a GraphQL query and returns its "data" member, as
            run_gh_graphql does

    Returns:
        A map from each ref to its state.  If GitHub did not answer a query,
        the state of each ref in that query is unknown.
    """
    unique_refs = list(dict.fromkeys(refs))
    states: dict[Ref, RefState] = {}
    for start in range(0, len(unique_refs), GITHUB_BATCH_SIZE):
        batch = unique_refs[start : start + GITHUB_BATCH_SIZE]
        fields = [ref_query_field(f"r{index}", ref) for index, ref in enumerate(batch)]
        try:
            data = run_graphql("query { " + " ".join(fields) + " }")
        except GraphQLError as error:
            for ref in batch:
                states[ref] = RefState(None, f"GitHub query failed: {error}")
            continue
        for index, ref in enumerate(batch):
            states[ref] = ref_state(ref, data.get(f"r{index}"))
    return states


# Gmail requests


def thread_ids(service: Any, query: str) -> list[str]:
    """Return the ids of the threads that match the given Gmail search, outside spam and trash.

    Args:
        service: a Gmail service
        query: a Gmail search, such as "from:notifications@github.com"

    Returns:
        The ids of the matching threads, as hexadecimal strings such as "1a19d2c2ea83367f".
    """
    ids: list[str] = []
    page_token = None
    while True:
        try:
            response = (
                service.users()
                .threads()
                .list(userId="me", q=query, pageToken=page_token, maxResults=500)
                .execute(num_retries=GMAIL_RETRIES)
            )
        except Exception as error:
            msg = f"Gmail search failed: {query}: {error}"
            raise RuntimeError(msg) from error
        ids.extend(thread["id"] for thread in response.get("threads", ()))
        page_token = response.get("nextPageToken")
        if not page_token:
            return ids


@dataclass(frozen=True)
class BatchResult:
    """The outcome of a set of Gmail requests.

    `responses` maps the id of each request that succeeded to its response, and
    `failures` maps the id of each request that failed to its last exception.
    """

    responses: dict[str, Any]
    failures: dict[str, Exception]


def _batch_pass(
    service: Any,
    requests: Mapping[str, Callable[[], Any]],
    request_ids: list[str],
    units: int,
    sleep: Callable[[float], None],
) -> BatchResult:
    """Return the outcome of executing the given Gmail requests once, in batches.

    Between batches, the routine waits long enough to stay within Gmail's quota.

    Args:
        service: a Gmail service
        requests: a map from a request id, such as a thread id, to a function that
            creates the request
        request_ids: the ids of the requests to execute
        units: the number of Gmail quota units that each request costs
        sleep: waits for the given number of seconds

    Returns:
        The responses to the requests that succeeded, and the exceptions of
        those that failed.
    """
    responses: dict[str, Any] = {}
    errors: dict[str, Exception] = {}

    def callback(request_id: str, response: Any, exception: Exception | None) -> None:
        if exception is None:
            responses[request_id] = response
        else:
            errors[request_id] = exception

    for start in range(0, len(request_ids), GMAIL_BATCH_SIZE):
        if start:
            sleep(GMAIL_BATCH_SIZE * units / GMAIL_QUOTA_UNITS_PER_SECOND)
        chunk = request_ids[start : start + GMAIL_BATCH_SIZE]
        batch = service.new_batch_http_request(callback=callback)
        for request_id in chunk:
            batch.add(requests[request_id](), request_id=request_id)
        try:
            batch.execute()
        except Exception as error:  # ruff: ignore[blind-except]
            # The whole batch failed, as for a network error.  Record the
            # exception for each request, so that batch_execute retries or reports it.
            for request_id in chunk:
                if request_id not in responses and request_id not in errors:
                    errors[request_id] = error
    return BatchResult(responses, errors)


def batch_execute(
    service: Any,
    requests: Mapping[str, Callable[[], Any]],
    units: int,
    sleep: Callable[[float], None] = time.sleep,
) -> BatchResult:
    """Return the outcome of executing the given Gmail requests, retrying transient failures.

    The routine executes the requests in batches.

    Args:
        service: a Gmail service
        requests: a map from a request id, such as a thread id, to a function that
            creates the request
        units: the number of Gmail quota units that each request costs
        sleep: waits for the given number of seconds

    Returns:
        The responses to the requests that succeeded, and the exceptions of
        those that failed permanently or that failed on every attempt.
    """
    responses: dict[str, Any] = {}
    permanent: dict[str, Exception] = {}
    errors: dict[str, Exception] = {}
    pending = list(requests)
    for attempt in range(GMAIL_RETRIES + 1):
        if attempt:
            sleep(2 ** (attempt - 1))
        result = _batch_pass(service, requests, pending, units, sleep)
        responses.update(result.responses)
        errors = result.failures
        permanent.update(
            (request_id, error)
            for request_id, error in errors.items()
            if not is_transient_api_error(error)
        )
        pending = [request_id for request_id in errors if request_id not in permanent]
        if not pending:
            break
    failures = permanent | {request_id: errors[request_id] for request_id in pending}
    return BatchResult(responses, failures)


def warn_failures(description: str, failures: Mapping[str, Exception]) -> None:
    """Warn about the given failed Gmail requests, if there are any.

    Args:
        description: what the requests do, such as "fetch thread"; used for diagnostics
        failures: a map from the id of each failed request to its exception
    """
    if not failures:
        return
    first, error = next(iter(failures.items()))
    others = f" and {len(failures) - 1} others" if len(failures) > 1 else ""
    warn(f"cannot {description} {first}{others}: {error}")


def thread_summaries(service: Any, ids: Iterable[str]) -> BatchResult:
    """Return the summaries of the given Gmail threads.

    Args:
        service: a Gmail service
        ids: distinct Gmail thread ids, such as "1a19d2c2ea83367f"

    Returns:
        A result whose `responses` maps the id of each thread that Gmail
        returned to its ThreadSummary, in the order of the ids.
    """
    threads = service.users().threads()

    def request(thread_id: str) -> Callable[[], Any]:
        return lambda: threads.get(
            userId="me", id=thread_id, format="metadata", metadataHeaders=["From", "Subject"]
        )

    ids = list(ids)
    result = batch_execute(service, {tid: request(tid) for tid in ids}, THREADS_GET_UNITS)
    summaries = {
        tid: ThreadSummary.from_gmail_thread(result.responses[tid])
        for tid in ids
        if tid in result.responses
    }
    return BatchResult(summaries, result.failures)


def trash_messages(service: Any, ids: Iterable[str]) -> dict[str, Exception]:
    """Move the given Gmail messages to the trash.

    Args:
        service: a Gmail service
        ids: Gmail message ids, such as "19a3c2f1b07d8e45"

    Returns:
        A map from the id of each message that Gmail did not trash to the exception.
    """
    messages = service.users().messages()

    def request(message_id: str) -> Callable[[], Any]:
        return lambda: messages.trash(userId="me", id=message_id)

    requests = {mid: request(mid) for mid in ids}
    return batch_execute(service, requests, MESSAGES_TRASH_UNITS).failures


# The main routine


@dataclass(frozen=True)
class Candidate:
    """A Gmail thread from a notification service."""

    source: Source
    thread: ThreadSummary


@dataclass(frozen=True)
class Search:
    """The threads that the sources' Gmail searches find.

    `candidates` holds the threads that Gmail returned, and `failures` maps the
    id of each thread that Gmail did not return to the exception.
    """

    candidates: list[Candidate]
    failures: dict[str, Exception]


def modify_account(config: AccountConfig) -> AccountConfig:
    """Return the given account, with a token cache for the Gmail modify scope.

    The modify scope needs its own token cache, because a cached token grants
    only the scopes that the user authorized when the cache was written.  The
    cache's file name is that of the account's cache, with "gmail-modify-"
    inserted, as in "googleusercontent-oauth-token-gmail-modify-mernst-cs-washington-edu.json".

    Args:
        config: an account

    Returns:
        The account, with a different token cache.

    Raises:
        RuntimeError: if the name of the account's cache does not contain
            "oauth-token-", so that the two caches would be the same file.
    """
    path = config.token_path
    name = path.name.replace("oauth-token-", "oauth-token-gmail-modify-", 1)
    if name == path.name:
        msg = f'cannot derive a modify-scope cache from {path}, which lacks "oauth-token-"'
        raise RuntimeError(msg)
    return dataclasses.replace(config, token_path=path.with_name(name))


def with_default_window(extra_query: str) -> str:
    """Return the given Gmail search terms, restricted to DEFAULT_WINDOW if they restrict no date.

    Args:
        extra_query: a string of Gmail search terms, such as "in:inbox", or ""

    Returns:
        The search terms, followed by DEFAULT_WINDOW if they contain no date operator.
    """
    if DATE_OPERATOR.search(extra_query):
        return extra_query
    return f"{extra_query} {DEFAULT_WINDOW}".strip()


def candidates(service: Any, extra_query: str) -> Search:
    """Return the threads that each source's Gmail search finds.

    Args:
        service: a Gmail service
        extra_query: Gmail search terms that restrict every source's search, or ""

    Returns:
        The threads, each paired with the first source whose search found it.
    """
    seen: set[str] = set()
    found: list[Candidate] = []
    failures: dict[str, Exception] = {}
    for source in SOURCES:
        query = f"{source.query} {extra_query}".strip()
        ids = [tid for tid in thread_ids(service, query) if tid not in seen]
        seen.update(ids)
        print(f"{source.name}: {len(ids)} threads")
        result = thread_summaries(service, ids)
        found.extend(Candidate(source, thread) for thread in result.responses.values())
        failures.update(result.failures)
    return Search(found, failures)


def main() -> int:
    """Report, and optionally trash, notification threads about closed pull requests and issues.

    Returns:
        The exit status: 0 if every Gmail request succeeded, and 1 otherwise.
    """
    parser = argparse.ArgumentParser(
        description="Move Gmail notifications about closed pull requests and issues to the trash."
    )
    parser.add_argument(
        "--delete", action="store_true", help="trash the threads, instead of only reporting them"
    )
    parser.add_argument(
        "--query",
        default="",
        help=(
            'Gmail search terms that restrict every search, as in "newer_than:7d" or "in:inbox";'
            f" if they restrict no date, {DEFAULT_WINDOW} is added"
        ),
    )
    accounts = {config.email: config for config in ACCOUNT_CONFIGS}
    parser.add_argument(
        "--account",
        choices=list(accounts),
        default=ACCOUNT_CONFIGS[0].email,
        help="the email address of the Gmail account to examine (default: %(default)s)",
    )
    arguments = parser.parse_args()

    account = modify_account(accounts[arguments.account])
    service = gmail_service(account, CLIENT_SECRET_PATH, GMAIL_MODIFY_SCOPE)
    search = candidates(service, with_default_window(arguments.query))
    warn_failures("fetch thread", search.failures)
    verdicts = thread_verdicts(search.candidates, ref_states)

    to_trash: list[Candidate] = []
    counts: Counter[Disposition] = Counter()
    for candidate, verdict in zip(search.candidates, verdicts, strict=True):
        counts[verdict.disposition] += 1
        subject = candidate.thread.subjects[0] if candidate.thread.subjects else "(no subject)"
        if verdict.disposition is Disposition.TRASH:
            to_trash.append(candidate)
            print(f"Trash: {subject}  [{verdict.description}]")
        elif verdict.disposition is Disposition.UNKNOWN:
            warn(f"keeping {subject}: {verdict.description}")

    kept = ", ".join(
        f"{counts[disposition]} {disposition.value}"
        for disposition in Disposition
        if disposition is not Disposition.TRASH and counts[disposition]
    )
    print(f"Kept {sum(counts.values()) - len(to_trash)} threads: {kept or 'none'}.")
    failed = bool(search.failures)
    if not to_trash:
        print("No threads to trash.")
    elif arguments.delete:
        failures = trash_messages(
            service, (mid for candidate in to_trash for mid in candidate.thread.message_ids)
        )
        warn_failures("trash message", failures)
        trashed = sum(
            1 for candidate in to_trash if failures.keys().isdisjoint(candidate.thread.message_ids)
        )
        print(f"Trashed {trashed} threads.")
        failed = failed or bool(failures)
    else:
        print(f"Would trash {len(to_trash)} threads; pass --delete to trash them.")
    return 1 if failed else 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        print("\nCancelled.")
    except RuntimeError as error:
        msg = f"mail-delete-closed-prs.py: {error}"
        raise SystemExit(msg) from error
