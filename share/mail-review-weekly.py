#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "browser-cookie3",
#   "google-api-python-client",
#   "google-auth",
#   "google-auth-oauthlib",
#   "requests",
# ]
# ///

"""Review weekly mail in Gmail.

Usage: mail-review-weekly.py

This script opens a search in a Gmail account only if the search has results
there.  See mail_review.py, in this directory, for its account configuration
and its authorization requirements.
"""

from __future__ import annotations

from mail_review import Review, Section, review_sections, start_sessions

# Every search names the labels or categories that it needs, so no prefix
# restricts them, and none of them is about spam or trash.
WEEKLY_REVIEW = Review()

WEEKLY_REVIEW_SECTIONS: tuple[Section, ...] = (
    Section(
        "Part 1: merged pull requests, kids' email, and specific promotions",
        (
            # Merged pull requests
            "label:pr-merged",
            # Many but not all of these are closed pull requests.
            (
                "label:inbox from:notifications@github.com "
                '"Reply to this email directly, view it on GitHub, or unsubscribe. '
                'You are receiving this because you are subscribed to this thread." '
                '"closed"'
            ),
            # Kids' daily & weekly email
            "category:promotions label:silas label:inbox",
            "category:social label:silas label:inbox",
            # "category:promotions label:maeve label:inbox",
            # "category:social label:maeve label:inbox",
            # Specific promotions email
            "from:scancafe label:inbox",
            "from:extracare label:inbox",
        ),
    ),
    Section(
        "Part 2: specific delayed mail",
        (
            # These come early so that the delay-weekly label might update
            # before I get to it.
            "label:delay-weekly seworld",
            "label:delay-weekly ecoop-info",
            'label:delay-junk "micheal"',
            "to:it-fmeurope-events",
        ),
    ),
    Section(
        "Part 3: weekly (and daily) email",
        (
            "label:delay-bulk",
            "label:delay-daily",
            "label:delay-junk",
            "label:delay-weekly",
        ),
    ),
    Section(
        "Part 4: social & promotions email",
        (
            "category:promotions label:inbox",
            "category:social label:inbox",
            "category:updates label:inbox label:commerce",
        ),
    ),
    Section(
        "Part 5: updates, progress reports, and forums",
        (
            "category:updates label:inbox",
            "to:progress-reports" + "@" + "cs.washington.edu label:inbox",
            "category:forums label:inbox",
        ),
    ),
)


def main() -> None:
    """Authenticate accounts and run the staged weekly review."""
    sessions = start_sessions()
    review_sections(WEEKLY_REVIEW_SECTIONS, sessions, WEEKLY_REVIEW)


if __name__ == "__main__":
    try:
        main()
    except (EOFError, KeyboardInterrupt):
        print("\nCancelled.")
    except RuntimeError as error:
        msg = f"mail-review-weekly.py: {error}"
        raise SystemExit(msg) from error
