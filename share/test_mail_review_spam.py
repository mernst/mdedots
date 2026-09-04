"""Tests for mail-review-spam that do not access Gmail or OAuth credentials."""

# The fake Google API objects intentionally implement only their public test surface.
# ruff: file-ignore[undocumented-public-method, undocumented-public-init, docstring-missing-returns, private-member-access]

from __future__ import annotations

import base64
import importlib.machinery
import importlib.util
import stat
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest import mock

SCRIPT_PATH = Path(__file__).with_name("mail-review-spam")
LOADER = importlib.machinery.SourceFileLoader("mail_review_spam", str(SCRIPT_PATH))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
assert SPEC is not None
mail_review_spam = importlib.util.module_from_spec(SPEC)
sys.modules[LOADER.name] = mail_review_spam
LOADER.exec_module(mail_review_spam)


class FakeRequest:
    """A fake executable Gmail API request."""

    def __init__(self, result=None, error=None):
        self.result = {} if result is None else result
        self.error = error

    def execute(self):
        if self.error is not None:
            raise self.error
        return self.result


class FakeMessages:
    """A fake users.messages resource that records list arguments."""

    def __init__(self, request):
        self.request = request
        self.list_arguments = None

    def list(self, **arguments):
        self.list_arguments = arguments
        return self.request


class FakeUsers:
    """A fake users resource."""

    def __init__(self, messages):
        self.messages_resource = messages

    def messages(self):
        return self.messages_resource


class FakeService:
    """A fake Gmail service."""

    def __init__(self, result=None, error=None):
        self.messages_resource = FakeMessages(FakeRequest(result, error))

    def users(self):
        return FakeUsers(self.messages_resource)


def session(email, slot, result=None, error=None):
    """Create a configured fake account session."""
    config = mail_review_spam.AccountConfig(email, Path("unused-token.json"))
    return mail_review_spam.AccountSession(config, slot, FakeService(result, error))


def protobuf_varint(value):
    """Encode a nonnegative protobuf varint."""
    encoded = bytearray()
    while value >= 0x80:
        encoded.append((value & 0x7F) | 0x80)
        value >>= 7
    encoded.append(value)
    return bytes(encoded)


def protobuf_field(number, value):
    """Encode a varint or length-delimited protobuf field."""
    if isinstance(value, int):
        return protobuf_varint(number << 3) + protobuf_varint(value)
    return protobuf_varint((number << 3) | 2) + protobuf_varint(len(value)) + value


def browser_account(email, *, valid=1, signed_out=0):
    """Return a synthetic account protobuf in Google's current format."""
    return b"".join(
        (
            protobuf_field(3, email.encode()),
            protobuf_field(9, valid),
            protobuf_field(10, b"gaia-id"),
            protobuf_field(14, signed_out),
        )
    )


def browser_payload(*accounts):
    """Wrap account protobufs in a base64 ListAccounts response."""
    response = b"".join(protobuf_field(1, account) for account in accounts)
    return base64.b64encode(response).decode()


class MailReviewSpamTest(unittest.TestCase):
    """Test API queries and browser-opening decisions."""

    @mock.patch.object(mail_review_spam.time, "sleep")
    @mock.patch.object(mail_review_spam.webbrowser, "open")
    def test_no_accounts_match_opens_no_tabs(self, browser_open, _sleep):
        sessions = (
            session("one@example.com", "0", {}),
            session("two@example.com", "2", {"messages": []}),
        )
        mail_review_spam.spam_search("subject:none", sessions)
        browser_open.assert_not_called()

    @mock.patch.object(mail_review_spam.time, "sleep")
    @mock.patch.object(mail_review_spam.webbrowser, "open")
    def test_only_matching_account_opens_encoded_url(self, browser_open, _sleep):
        sessions = (
            session("one@example.com", "0", {}),
            session("two@example.com", "2", {"messages": [{"id": "1"}]}),
        )
        mail_review_spam.spam_search('subject:"Müller test" to:a@example.com', sessions)
        browser_open.assert_called_once_with(
            "https://mail.google.com/mail/u/2/#search/"
            "in%3Aspam%20subject%3A%22M%C3%BCller%20test%22%20to%3Aa%40example.com"
        )

    @mock.patch.object(mail_review_spam.time, "sleep")
    @mock.patch.object(mail_review_spam.webbrowser, "open")
    def test_both_accounts_match_in_account_order(self, browser_open, _sleep):
        sessions = (
            session("one@example.com", "0", {"messages": [{"id": "1"}]}),
            session("two@example.com", "2", {"messages": [{"id": "2"}]}),
        )
        mail_review_spam.spam_search("(from:a OR from:b)", sessions)
        self.assertEqual(
            [call.args[0].split("/u/")[1][0] for call in browser_open.call_args_list],
            ["0", "2"],
        )

    def test_api_request_checks_one_spam_message(self):
        service = FakeService({"messages": [{"id": "1"}]})
        self.assertTrue(mail_review_spam.search_has_messages(service, 'subject:"hello"'))
        self.assertEqual(
            service.messages_resource.list_arguments,
            {
                "userId": "me",
                "q": 'in:spam subject:"hello"',
                "includeSpamTrash": True,
                "maxResults": 1,
            },
        )

    def test_browser_account_positions_become_slots(self):
        payload = browser_payload(
            browser_account("first@example.com"),
            browser_account("signed-out@example.com", signed_out=1),
            browser_account("THIRD@example.com"),
        )
        self.assertEqual(
            mail_review_spam.parse_browser_accounts(payload),
            {"first@example.com": "0", "third@example.com": "2"},
        )

    def test_changed_browser_response_fails_loudly(self):
        with self.assertRaisesRegex(ValueError, "not valid base64"):
            mail_review_spam.parse_browser_accounts("obsolete JSON response")

    def test_missing_expected_browser_account_requires_rewrite(self):
        class FakeResponse:
            text = browser_payload(browser_account("other@example.com"))

            def raise_for_status(self):
                pass

        fake_browser_cookie3 = types.SimpleNamespace(chrome=lambda **_arguments: object())
        fake_requests = types.SimpleNamespace(post=lambda *_args, **_arguments: FakeResponse())
        with (
            mock.patch.dict(
                sys.modules,
                {"browser_cookie3": fake_browser_cookie3, "requests": fake_requests},
            ),
            self.assertRaisesRegex(RuntimeError, "must be rewritten"),
        ):
            mail_review_spam.discover_browser_slots(("expected@example.com",))

    def test_browser_account_request_matches_chromium(self):
        cookie_jar = object()
        calls = []

        class FakeResponse:
            text = browser_payload(browser_account("expected@example.com"))

            def raise_for_status(self):
                pass

        def post(*args, **arguments):
            calls.append((args, arguments))
            return FakeResponse()

        fake_browser_cookie3 = types.SimpleNamespace(chrome=lambda **_arguments: cookie_jar)
        fake_requests = types.SimpleNamespace(post=post)
        with mock.patch.dict(
            sys.modules,
            {"browser_cookie3": fake_browser_cookie3, "requests": fake_requests},
        ):
            self.assertEqual(
                mail_review_spam.discover_browser_slots(("expected@example.com",)),
                {"expected@example.com": "0"},
            )

        self.assertEqual(calls[0][0], (mail_review_spam.LIST_ACCOUNTS_URL,))
        self.assertEqual(
            calls[0][1],
            {
                "data": " ",
                "headers": {
                    "Content-Type": "application/x-www-form-urlencoded",
                    "Origin": "https://www.google.com",
                },
                "cookies": cookie_jar,
                "timeout": 10,
            },
        )

    @mock.patch.object(mail_review_spam.time, "sleep")
    @mock.patch.object(mail_review_spam.webbrowser, "open")
    def test_transient_error_opens_tab_with_warning(self, browser_open, _sleep):
        class TransientError(Exception):
            resp = type("Response", (), {"status": 503})()

        mail_review_spam.spam_search(
            "subject:test", (session("one@example.com", "0", error=TransientError()),)
        )
        browser_open.assert_called_once()

    @mock.patch.object(mail_review_spam.webbrowser, "open")
    def test_permanent_error_stops_without_opening_tab(self, browser_open):
        with self.assertRaisesRegex(RuntimeError, "Gmail search failed for one@example.com"):
            mail_review_spam.spam_search(
                "subject:test", (session("one@example.com", "0", error=ValueError("bad")),)
            )
        browser_open.assert_not_called()

    def test_token_file_is_private(self):
        with tempfile.TemporaryDirectory() as directory:
            token_path = Path(directory) / "tokens" / "account.json"
            mail_review_spam._write_private_token(token_path, "secret")
            self.assertEqual(token_path.read_text(encoding="utf-8"), "secret")
            self.assertEqual(stat.S_IMODE(token_path.stat().st_mode), 0o600)


if __name__ == "__main__":
    unittest.main()
