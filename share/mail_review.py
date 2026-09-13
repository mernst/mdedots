"""Shared support for the Gmail review scripts.

mail-review-spam.py and mail-review-weekly.py import this module, which must
remain in the same directory as those scripts.  Each script declares the dependencies that
these routines need; run a script directly or with `uv run --script`, so that uv
supplies them.

The Gmail API must be enabled for the application represented by
~/private/googleusercontent-oauth-client-secret.json.  On the first run, a
script asks the user to authorize each account; later runs use separate token
caches in ~/private.
"""

# Each script's uv header supplies the third-party libraries, which this file
# imports where they are used, so that running a script without them explains
# how to supply them.  They are not installed in this project's environment,
# which is why every such import also suppresses ty's unresolved-import error.
# ruff: file-ignore[import-outside-top-level]

from __future__ import annotations

import base64
import binascii
import contextlib
import sys
import tempfile
import time
import webbrowser
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from urllib.parse import quote


@dataclass(frozen=True)
class AccountConfig:
    """A Gmail identity and its OAuth token cache."""

    email: str
    token_path: Path


ACCOUNT_CONFIGS = (
    AccountConfig(
        "mernst" + "@" + "cs.washington.edu",
        Path.home() / "private/googleusercontent-oauth-token-mernst-cs-washington-edu.json",
    ),
    AccountConfig(
        "michael.ernst" + "@" + "gmail.com",
        Path.home() / "private/googleusercontent-oauth-token-michael-ernst-gmail-com.json",
    ),
)


GMAIL_READONLY_SCOPE = "https://www.googleapis.com/auth/gmail.readonly"
# This identifies the application; by contrast, the oauth *tokens* identify an account.
CLIENT_SECRET_PATH = Path.home() / "private/googleusercontent-oauth-client-secret.json"
GMAIL_URL_PREFIX = "https://mail.google.com/mail/u/"
LIST_ACCOUNTS_URL = (
    "https://accounts.google.com/ListAccounts"
    "?gpsia=1&source=ChromiumBrowser&json=standard&laf=b64bin"
)
DISCOVERY_FAILURE = (
    "Google browser-account discovery no longer works; mail_review.py must be rewritten"
)
MISSING_LIBRARIES = (
    "the mail-review scripts require browser-cookie3, google-api-python-client, google-auth, "
    "google-auth-oauthlib, and requests; execute the script directly or with "
    "`uv run --script` so uv supplies them"
)
# Delay after opening a browser tab, to avoid overwhelming the browser.
TAB_DELAY_SECONDS = 0.1


@dataclass(frozen=True)
class AccountSession:
    """An account configuration paired with an authenticated Gmail service."""

    config: AccountConfig
    browser_slot: str
    service: Any


@dataclass(frozen=True)
class Review:
    """How one review script turns its search terms into Gmail queries.

    `query_prefix` restricts every search term of the review, and
    `include_spam_trash` determines whether the Gmail API also considers
    messages in the spam and trash folders, which it ignores by default.
    """

    query_prefix: str = ""
    include_spam_trash: bool = False

    def query(self, search_term: str) -> str:
        """Restrict a search term to the part of the mailbox that this review covers.

        Returns:
            The complete Gmail query for the search term.
        """
        return f"{self.query_prefix} {search_term}" if self.query_prefix else search_term


@dataclass(frozen=True)
class Section:
    """A group of searches that the user reviews before continuing."""

    heading: str
    searches: tuple[str, ...]


def warn(message: str) -> None:
    """Print a warning that is not part of the review's ordinary output."""
    print(f"Warning: {message}", file=sys.stderr)


def _read_varint(data: bytes, position: int) -> tuple[int, int]:
    """Read one protobuf varint.

    Returns:
        The decoded integer and the position following it.
    """
    result = 0
    for shift in range(0, 70, 7):
        if position >= len(data):
            raise ValueError("truncated protobuf varint")
        byte = data[position]
        position += 1
        result |= (byte & 0x7F) << shift
        if not byte & 0x80:
            return result, position
    raise ValueError("oversized protobuf varint")


def _parse_protobuf_fields(data: bytes) -> list[tuple[int, int, int | bytes]]:
    """Parse the protobuf wire types used by ListAccounts.

    Returns:
        Tuples containing field number, wire type, and field value.
    """
    fields: list[tuple[int, int, int | bytes]] = []
    position = 0
    while position < len(data):
        key, position = _read_varint(data, position)
        number, wire_type = key >> 3, key & 7
        if number == 0:
            raise ValueError("invalid protobuf field number zero")
        if wire_type == 0:
            value, position = _read_varint(data, position)
        elif wire_type == 1:
            end = position + 8
            if end > len(data):
                raise ValueError("truncated fixed64 protobuf field")
            value, position = data[position:end], end
        elif wire_type == 2:
            length, position = _read_varint(data, position)
            end = position + length
            if end > len(data):
                raise ValueError("truncated length-delimited protobuf field")
            value, position = data[position:end], end
        elif wire_type == 5:
            end = position + 4
            if end > len(data):
                raise ValueError("truncated fixed32 protobuf field")
            value, position = data[position:end], end
        else:
            message = f"unsupported protobuf wire type {wire_type}"
            raise ValueError(message)
        fields.append((number, wire_type, value))
    return fields


def _single_field(
    fields: list[tuple[int, int, int | bytes]], number: int, wire_type: int
) -> int | bytes | None:
    """Get an optional non-repeated protobuf field with the expected wire type.

    `fields` comes from `_parse_protobuf_fields`, so a wire-type 0 value is a
    varint that `_read_varint` decoded from the ListAccounts payload, and every
    other wire type carries the raw bytes of the field.

    Returns:
        The field value, or None when the field is absent.
    """
    matching = [value for field, wire, value in fields if field == number and wire == wire_type]
    wrong_wire = any(field == number and wire != wire_type for field, wire, _value in fields)
    if wrong_wire or len(matching) > 1:
        message = f"invalid or repeated protobuf field {number}"
        raise ValueError(message)
    return matching[0] if matching else None


def parse_browser_accounts(payload: str) -> dict[str, str]:
    """Parse Google's undocumented base64-protobuf browser-account response.

    Returns:
        A mapping from valid signed-in email addresses to Gmail browser slots.
    """
    if not payload.strip():
        raise ValueError("empty ListAccounts response")
    try:
        decoded = base64.b64decode(payload.strip(), validate=True)
    except (binascii.Error, ValueError) as error:
        raise ValueError("ListAccounts response is not valid base64") from error

    top_level_fields = _parse_protobuf_fields(decoded)
    if any(number == 1 and wire_type != 2 for number, wire_type, _ in top_level_fields):
        raise ValueError("invalid ListAccounts account field")
    account_messages = [
        value
        for number, wire_type, value in top_level_fields
        if number == 1 and wire_type == 2 and isinstance(value, bytes)
    ]
    if not account_messages:
        raise ValueError("ListAccounts response contains no accounts")

    result: dict[str, str] = {}
    seen_emails: set[str] = set()
    for index, account_message in enumerate(account_messages):
        fields = _parse_protobuf_fields(account_message)
        email_bytes = _single_field(fields, 3, 2)
        gaia_id = _single_field(fields, 10, 2)
        valid_session = _single_field(fields, 9, 0)
        signed_out = _single_field(fields, 14, 0)
        if not isinstance(email_bytes, bytes) or not email_bytes:
            message = f"missing email in ListAccounts record at position {index}"
            raise ValueError(message)
        if not isinstance(gaia_id, bytes) or not gaia_id:
            message = f"missing Gaia ID in ListAccounts record at position {index}"
            raise ValueError(message)
        if valid_session not in (None, 0, 1) or signed_out not in (None, 0, 1):
            message = f"invalid account state in ListAccounts record at position {index}"
            raise ValueError(message)
        try:
            email = email_bytes.decode("utf-8").casefold()
            gaia_id.decode("utf-8")
        except UnicodeDecodeError as error:
            message = f"invalid text in ListAccounts record at position {index}"
            raise ValueError(message) from error
        if email in seen_emails:
            message = f"duplicate browser account {email}"
            raise ValueError(message)
        seen_emails.add(email)
        # The Gmail /u/N/ slot is the account's position in the full list, so
        # signed-out accounts still occupy a position.
        if valid_session != 0 and signed_out != 1:
            result[email] = str(index)
    return result


def discover_browser_slots(expected_emails: tuple[str, ...]) -> dict[str, str]:
    """Discover Gmail slots using cookies from Chrome's signed-in session.

    Returns:
        A mapping from each expected email address to its current numeric slot.
    """
    try:
        import browser_cookie3  # ty: ignore[unresolved-import]
        import requests  # ty: ignore[unresolved-import]
    except ImportError as error:
        raise RuntimeError(MISSING_LIBRARIES) from error

    try:
        cookies = browser_cookie3.chrome(domain_name=".google.com")
        response = requests.post(
            LIST_ACCOUNTS_URL,
            data=" ",
            headers={
                "Content-Type": "application/x-www-form-urlencoded",
                "Origin": "https://www.google.com",
            },
            cookies=cookies,
            timeout=10,
        )
        response.raise_for_status()
        discovered = parse_browser_accounts(response.text)
        slots: dict[str, str] = {}
        for email in expected_emails:
            slot = discovered.get(email.casefold())
            if slot is None:
                message = f"expected signed-in account is missing or invalid: {email}"
                raise ValueError(message)  # ruff: ignore[raise-within-try]
            slots[email.casefold()] = slot
        return slots
    except Exception as error:
        message = f"{DISCOVERY_FAILURE}: {error}"
        raise RuntimeError(message) from error


def _write_private_token(path: Path, contents: str) -> None:
    """Atomically write an OAuth token cache readable only by its owner."""
    # `mkdir` honors `mode` only when it creates the directory, so an existing
    # directory keeps the permissions it already has.  The token file itself is
    # 0600, so no other user can read it; but another user who can write the
    # directory can replace the token file with one of their own choosing, and
    # another user who can read it can see the token file names.
    if path.parent.is_dir():
        directory_mode = path.parent.stat().st_mode
        if directory_mode & 0o022:
            warn(f"{path.parent} is writable by other users; they can replace token files")
        elif directory_mode & 0o055:
            warn(f"{path.parent} is accessible to other users; token file names are visible")
    else:
        path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    temporary_name: str | None = None
    try:
        with tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", dir=path.parent, prefix=path.name + ".", delete=False
        ) as output:
            temporary_name = output.name
            Path(temporary_name).chmod(0o600)
            output.write(contents)
        Path(temporary_name).replace(path)
        temporary_name = None
        path.chmod(0o600)
    finally:
        if temporary_name is not None:
            with contextlib.suppress(FileNotFoundError):
                Path(temporary_name).unlink()


def cached_credentials(config: AccountConfig) -> Any | None:
    """Load an account's cached OAuth credentials, refreshing them if they have expired.

    Returns:
        Usable credentials, or None if the user must authorize the account again.
    """
    if not config.token_path.exists():
        return None
    try:
        from google.auth.exceptions import GoogleAuthError  # ty: ignore[unresolved-import]
        from google.auth.transport.requests import Request  # ty: ignore[unresolved-import]
        from google.oauth2.credentials import Credentials  # ty: ignore[unresolved-import]
    except ImportError as error:
        raise RuntimeError(MISSING_LIBRARIES) from error

    try:
        credentials = Credentials.from_authorized_user_file(
            str(config.token_path), [GMAIL_READONLY_SCOPE]
        )
        if not credentials.valid and credentials.expired and credentials.refresh_token:
            credentials.refresh(Request())
    except (GoogleAuthError, OSError, ValueError) as error:
        # The cache is unreadable or malformed, or its refresh token has
        # expired or been revoked.  Authorizing the account again fixes it.
        warn(f"ignoring unusable cached credentials in {config.token_path}: {error}")
        return None
    return credentials if credentials.valid else None


def authenticate_account(
    config: AccountConfig, browser_slot: str, client_secret_path: Path
) -> AccountSession:
    """Authenticate one account and verify that Google returned the expected identity.

    Returns:
        The authenticated account session.
    """
    try:
        from google_auth_oauthlib.flow import InstalledAppFlow  # ty: ignore[unresolved-import]
        from googleapiclient.discovery import build  # ty: ignore[unresolved-import]
    except ImportError as error:
        raise RuntimeError(MISSING_LIBRARIES) from error

    try:
        credentials = cached_credentials(config)
        from_cache = credentials is not None
        if credentials is None:
            # Check this before asking the user to authorize, so that a missing
            # file is not reported only after the user has waited in a browser.
            if not client_secret_path.is_file():
                msg = f"OAuth client-secret file does not exist: {client_secret_path}"
                raise RuntimeError(msg)  # ruff: ignore[raise-within-try]
            print(f"Authorize {config.email} (Gmail browser slot {browser_slot}).")
            flow = InstalledAppFlow.from_client_secrets_file(
                str(client_secret_path), [GMAIL_READONLY_SCOPE]
            )
            credentials = flow.run_local_server(
                port=0,
                prompt="select_account",
                authorization_prompt_message="",
            )

        if not credentials.valid:
            raise RuntimeError(  # ruff: ignore[raise-within-try]
                "the OAuth authorization did not produce valid credentials"
            )

        service = build("gmail", "v1", credentials=credentials, cache_discovery=False)
        actual_email = service.users().getProfile(userId="me").execute()["emailAddress"]
        if actual_email.casefold() != config.email.casefold():
            if from_cache:
                msg = (
                    f"cached credentials in {config.token_path} belong to {actual_email}, "
                    f"but {config.email} was expected; delete that file and authorize again"
                )
            else:
                msg = (
                    f"authorized {actual_email}, but expected {config.email}; "
                    f"{config.token_path} was not written"
                )
            raise RuntimeError(msg)  # ruff: ignore[raise-within-try]
        _write_private_token(config.token_path, credentials.to_json())
        return AccountSession(config, browser_slot, service)
    except RuntimeError:
        raise
    except Exception as error:
        msg = f"cannot authorize {config.email}: {error}"
        raise RuntimeError(msg) from error


def authenticate_accounts(
    browser_slots: dict[str, str],
    configs: tuple[AccountConfig, ...] = ACCOUNT_CONFIGS,
    client_secret_path: Path = CLIENT_SECRET_PATH,
) -> tuple[AccountSession, ...]:
    """Authenticate the given Gmail accounts.

    Returns:
        Authenticated sessions in configuration order.
    """
    return tuple(
        authenticate_account(config, browser_slots[config.email.casefold()], client_secret_path)
        for config in configs
    )


def start_sessions(
    configs: tuple[AccountConfig, ...] = ACCOUNT_CONFIGS,
    client_secret_path: Path = CLIENT_SECRET_PATH,
) -> tuple[AccountSession, ...]:
    """Find each account's current browser slot and authenticate it.

    Returns:
        Authenticated sessions in configuration order.
    """
    browser_slots = discover_browser_slots(tuple(config.email for config in configs))
    return authenticate_accounts(browser_slots, configs, client_secret_path)


def gmail_search_url(browser_slot: str, query: str) -> str:
    """Return the Gmail browser URL for a search.

    Returns:
        A URL with the complete search query encoded in its fragment.
    """
    return f"{GMAIL_URL_PREFIX}{browser_slot}/#search/{quote(query, safe='')}"


def gmail_folder_url(browser_slot: str, folder: str) -> str:
    """Return the Gmail browser URL for a folder such as "spam".

    Returns:
        A URL that shows one folder in one account.
    """
    return f"{GMAIL_URL_PREFIX}{browser_slot}/#{quote(folder, safe='')}"


def open_tab(url: str) -> None:
    """Open one Gmail URL in the browser."""
    if not webbrowser.open(url):
        warn(f"no browser accepted {url}")
    time.sleep(TAB_DELAY_SECONDS)


def search_succeeds(service: Any, query: str, include_spam_trash: bool) -> bool:
    """Return whether a Gmail search has at least one result.

    Returns:
        Whether at least one matching message exists.
    """
    response = (
        service.users()
        .messages()
        .list(
            userId="me",
            q=query,
            includeSpamTrash=include_spam_trash,
            maxResults=1,
        )
        .execute()
    )
    return bool(response.get("messages"))


def is_transient_api_error(error: Exception) -> bool:
    """Return whether an API error is likely to be temporary.

    Returns:
        Whether the same request is likely to succeed if retried.
    """
    status = getattr(getattr(error, "resp", None), "status", None)
    if status in (408, 429) or (isinstance(status, int) and 500 <= status <= 599):
        return True
    # Gmail reports both throttling and permanent authorization failures as 403.
    if status == 403 and "ratelimitexceeded" in str(error).casefold():
        return True
    # OSError covers the connection, timeout, name-resolution, and TLS errors
    # that the HTTP client raises.
    try:
        from google.auth.exceptions import TransportError  # ty: ignore[unresolved-import]
    except ImportError:
        return isinstance(error, OSError)
    return isinstance(error, (TransportError, OSError))


def open_search(search_term: str, sessions: tuple[AccountSession, ...], review: Review) -> None:
    """Open this search only for accounts in which it has results."""
    query = review.query(search_term)
    for session in sessions:
        try:
            has_messages = search_succeeds(session.service, query, review.include_spam_trash)
        except Exception as error:
            if not is_transient_api_error(error):
                msg = f"Gmail search failed for {session.config.email}: {query}: {error}"
                raise RuntimeError(msg) from error
            # For a temporary failure, opening the tab is better than
            # terminating the review; at worst the tab shows no messages.
            warn(
                f"Gmail API unavailable for {session.config.email}; "
                f"opening search without checking: {query}"
            )
            has_messages = True

        if has_messages:
            open_tab(gmail_search_url(session.browser_slot, query))


def review_section(section: Section, sessions: tuple[AccountSession, ...], review: Review) -> None:
    """Open the section's searches that have results, then wait for the user to review them."""
    print(section.heading)
    for search_term in section.searches:
        open_search(search_term, sessions, review)
    input("Press ENTER")


def review_sections(
    sections: tuple[Section, ...], sessions: tuple[AccountSession, ...], review: Review
) -> None:
    """Work through every section of a review, one at a time."""
    for section in sections:
        review_section(section, sessions, review)
