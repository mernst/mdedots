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

"""Review spam mail in Gmail.

Usage: mail-review-spam.py

This script opens a search in a Gmail account only if the search has results
there.  See mail_review.py, in this directory, for its account configuration
and its authorization requirements.
"""

from __future__ import annotations

from mail_review import Review, Section, gmail_folder_url, open_tab, review_sections, start_sessions

# Gmail ignores spam and trash unless a search requests them.
SPAM_REVIEW = Review(query_prefix="in:spam", include_spam_trash=True)

GERMAN_TERMS = (
    '"Geschäft"',
    '"Sehr geehrter"',
    '"und mit"',
    '"nur vom"',
    '"haben Sie"',
    '"wie geht es dir"',
    '"mit der"',
    '"Herzlichen"',
    '"Glückwunsch"',
    "ich",
    '("wir" AND "die")',
    '("ab" AND "im")',
    '("und" AND "von")',
    '("ist" AND "diese")',
    '"guten tag"',
    '"spende"',
    '"wir uns"',
    '("Ihre" AND "ist")',
    '(Gutschein OR "kannst du")',
    '("hallo" AND "ist")',
    '("Rückfrage" AND "zu")',
    '("Ihr Konto")',
    '("ich bin")',
    '("ich habe")',
    "(Nachricht)",
    "(kurze Frage)",
    "(anfrage zu)",
    "(frage zu)",
)
GERMAN_SEARCH = "(" + " OR ".join(GERMAN_TERMS) + ")"

# To avoid "Showing related results", double-quote the term.
SPAM_REVIEW_SECTIONS: tuple[Section, ...] = (
    Section(
        "Mail forged to be from me",
        ("(from:michaelernst OR from:michael.ernst OR from:mernst)",),
    ),
    Section(
        "My username or name in the subject line",
        (
            '(subject:"michaelernst" OR subject:"mernst" OR subject:"cs.washington.edu")',
            # also catches "Michael Ernst"
            '(subject:"michael.ernst" OR subject:"Michael D Ernst")',
        ),
    ),
    Section(
        '"Michael" in the subject line or "Hi Michael"',
        (
            "subject:Michael",
            '"Hi Michael"',
        ),
    ),
    Section(
        'Misspelled "Micheal"',
        ('"Micheal"',),
    ),
    Section(
        "Likely false positives (by sender, part 1)",
        (
            "from:amazon.com",
            "from:auto-reply" + "@" + "usps.com",
            "from:builds" + "@" + "circleci.com",
            "from:centurylink.com",
            "from:checker-framework-dev",
            "from:cs.washington.edu",
            "from:fredhutch.org",
            "from:hallowelltodaro.com",
            "from:info" + "@" + "mountaineers.org",
            "from:jsagarin" + "@" + "verizon.net",
            "from:lincolnseattleptsa.org",
            "from:lls.org",
        ),
    ),
    Section(
        "Likely false positives (by sender, part 2)",
        (
            "from:mygoodtogo.com",
            "from:newyorktimes.com",
            "from:noreply" + "@" + "steampowered.com",
            "from:pnc.com",
            "from:seattlecca.org",
            "from:spl.org",
            "from:uw.edu",
            "from:washington.edu",
            'from:"Google Calendar"',
            'from:"Mail Delivery System"',
            'from:"Mail Delivery Subsystem"',
        ),
    ),
    Section(
        "Likely false positives (other than by sender)",
        (
            'subject:"security alert for"',
            "subject:seajug",
            "subject:seworld",
            "subject:typetools",
            "to:drool" + "@" + "mit.edu",
            "to:eit" + "@" + "mit.edu",
            "to:mit1989" + "@" + "mailman-alum.mit.edu",
            # "to:sgcyouth" + "@" + "googlegroups.com",
            '"Allen School"',
            "ecoop-info",
            "lombok",
            "maeveahowell" + "@" + "gmail.com",
            '"plse"',
            "uw-security-research",
        ),
    ),
    Section(
        "Possible false positive for calendar event",
        ("subject:invitation",),
    ),
    Section(
        'Foreign languages (watch out for "Showing related results")',
        (
            # # These emoji searches do not work; the search matches all emails.
            # "💋",
            # "❤",
            # "💘",
            # # It works if I abut it with a word, like this:
            # # "🔥H0T",
            #
            # German
            GERMAN_SEARCH,
            # Portuguese
            (
                '"Ação" OR Requerida OR "Atualização" OR Importante OR Notificacao OR '
                "aprovação OR reclamação OR eletrônica OR curriculo OR Investigação"
            ),
            # Spanish
            '(mejor AND opción) OR ("en su")',
            # Russian
            "и",
            # Polish
            "Proszę OR Słowa OR Dzień OR związku OR Wsparcie OR programistyczne OR spotkania",
            # Italian
            "Fattura OR pagata OR guardare OR signora OR uomo",
            # Japanese
            "様",
            # Chinese or Japanese
            "重要",
            # NOTE: Most of these aren't a problem because the email subject sorts
            # at the end when I review the email in Emacs Mew.
            # "К",  # ruff:ignore[ambiguous-unicode-character-comment] # intentionally not regular "K"
            # "в",
            # "я",
            # "في",
            # "مارس",
        ),
    ),
    Section(
        "Suspicious senders, part 1",
        (
            ## Too broad
            # "from:mail",
            ## Had no search results for 3 months
            # 'from:"Mustafa Ayvaz"',
            # 'from:"Ted\'s Wood Working"',
            # 'from:"ZEISS Microscopy"',
            # "from:GreenNewDealNetwork.org",
            # "from:Plantronics",
            # "from:isualum" + "@" + "mail.iastate.edu",
            # '"Scoop News"', # nothing on 8/3/2022, 9/5/2022
            # "CyberScoop", # nothing on 8/3/2022, 9/5/2022
            # "EdScoop", # nothing on 8/3/2022, 9/5/2022
            # "FedScoop", # nothing on 8/3/2022, 9/5/2022
            # "StateScoop", # nothing on 8/3/2022, 9/5/2022
            # "Fluke", # nothing on 8/3/2022, 9/5/2022
            # "Pluralsight", # nothing on 8/3/2022, 9/5/2022
            # "TeaParty.org", # nothing on 8/3/2022, 9/5/2022
            # "angi", # nothing on 8/3/2022, 9/5/2022
            # "cyberweek", # nothing on 8/3/2022, 9/5/2022
            "(from:dr OR from:mr OR from:mrs OR from:ms)",
            'from:"Anand Raghavan"',  # nothing on 9/5/2022 # nothing on 8/3/2023
            'from:"Cooling Bra Pro"',  # nothing on 9/5/2022
            'from:"EMILYs List"',  # nothing on 8/3/2023
            'from:"Fluke Corporation"',  # nothing on 8/3/2023
            'from:"Harbor Freight"',  # nothing on 8/3/2023
            'from:"IKA Works"',
            'from:"Innovation News Network"',
            'from:"Mend The Marriage"',  # nothing on 8/3/2023
            'from:"Michael Weber"',
            'from:"Survival Ops Tactical"',  # nothing on 8/3/2023
            'from:"TPC Training"',  # nothing on 8/3/2023
            'from:"Team OutSystems"',  # nothing on 8/3/2023
            'from:"Tracy Zettinig"',  # nothing on 8/3/2023
            'from:"Windstream Enterprise"',  # nothing on 8/3/2023
            "from:8x8",  # nothing on 8/3/2023
            "from:webinars" + "@" + "training.businesswatchnetwork.com",
            "from:expertspeak" + "@" + "email.concordeducations.com",
        ),
    ),
    Section(
        "Suspicious senders, part 2",
        (
            "from:AlertMedia",
            "from:Amazon",
            "from:BOXX",
            "from:Banco",
            "from:BeTechly",
            "from:Bloomberg",
            "from:EDIBON",
            "from:ESET",
            "from:Egnyte",
            "from:Ginja",
            "from:Helpdesk",
            "from:Hitradio",
            "from:McAfee",
            "from:Norton",
            "from:Planview",
            "from:RingCentral",
            "from:SeguridadHB",
            "from:Avantor",
            "from:ATC-NY",
            "from:SimpsonScarborough",
            "from:VinFuture",
            'from:"Check Point"',
            'from:"Staples Technology Solutions"',
            "from:FB",
            'from:"Amalia Meyer"',
            'from:"Anna Müller"',
            'from:"AAA"',
            'from:"topgolf"',
            'from:"Open Access Government"',
            'from:"Horst Thomas"',
        ),
    ),
    Section(
        "Suspicious senders, part 3",
        (
            # "from:bee" + "@" + "BAUER.UH.EDU",  # nothing on 8/3/2023
            "from:jooble.org",
            "from:deuscustoms.com",
            "from:blr",
            "from:cardio",
            "from:convergetp.com",
            "from:demings",
            "from:fortinet",
            "from:imperva",
            "from:liberator",
            "from:linkedin",
            "from:sci_ed" + "@" + "info.vwr.com",
            "from:sonicwall",
            "from:uCertify",
            "from:whitmer",
            "from:CashApp",
            "from:temu",
            "from:howida",
            "from:email",
            "from:bytesize",
            "from:nfina",
            'from:"Klaus Richter"',
            'from:"Manfred Böhm"',
            'from:"Manfred Schmitz"',
            "bara" + "@" + "kolliers.com",
        ),
    ),
    Section(
        "Suspicious senders, part 4",
        (
            "from:no-reply" + "@" + "calendly.com",
            'from:"The Flights Guru"',
            "from:cognella.com",
            "from:matscience-summit.com",
            "from:medtechmvp",
            "from:medlinepapersoa.online",
            "from:editor",
            "from:journal",
            "from:team",
            "from:noreply" + "@" + "reply.kik-textilien.eu",
        ),
    ),
    Section(
        "Suspicious senders, part 5",
        (
            # These do not include "from:" in the search
            "keynote",
            "anyworkanywhere",
            '"golf wire"',
            "Speechelo",
            "Syxsense",
            "labelbox",
            "techwatch",
        ),
    ),
    Section(
        "Frequent topics 1",
        (
            "subject:urgent",
            "Scopus",
            "subject:journal",
            '"nick hellen"',
            '"michael duwayne"',
        ),
    ),
    Section(
        "Frequent topics 2",
        (
            ## These are not so useful
            # "Seattle",
            # "democratic",
            # "republican",
            # "subject:Trump",
            ## These did not occur for 3 months
            # "CBD",
            # "Imperva",
            # "KN95",
            # "fuckbuddy",
            # "hookup",
            # "pussy",
            # "temperature",
            "singles",
            '"sam\'s club"',
            '"tractor supply"',
            '"Sky Devialet Soundbox"',
            "diabetes",
            "tinnitus",
            "subject:donation",
            "subject:investment",
            "subject:quota",
            "subject:spam",
            "subject:strategies",
        ),
    ),
    Section(
        "Frequent topics 3",
        (
            "subject:confirmation",
            "subject:conference",
            "Casino",
            "(bitcoin OR crypto)",
            "(coronavirus OR covid)",
            "fuckme",
            "indonesia",  # nothing on 2/3/2023
            "(keto OR ketosis)",  # nothing on 8/3/2022
            "docusign",
            "elongation",
            "trump",
            # "Kamala Harris",
        ),
    ),
    Section(
        "To unused email address",
        (
            "to:mernst" + "@" + "cs.rice.edu",
            "to:mernst" + "@" + "rice.edu",
        ),
    ),
)


def main() -> None:
    """Authenticate accounts and run the staged spam review."""
    print("Stop and re-start Chrome; probably requires `killall chrome` on the command line")
    input("Press ENTER")
    sessions = start_sessions()

    review_sections(SPAM_REVIEW_SECTIONS, sessions, SPAM_REVIEW)

    print("Now the rest (read these in Emacs, on the same machine running a browser).")
    print("1. Sort by from, look for frequent senders with non-uniform subject lines.")
    print("2. Sort by subject, scan for non-spam.")
    print("3. Delete all spam from Gmail.")
    input("Restart the web browser, then press ENTER")
    for session in sessions:
        open_tab(gmail_folder_url(session.browser_slot, "spam"))


if __name__ == "__main__":
    try:
        main()
    except (EOFError, KeyboardInterrupt):
        print("\nCancelled.")
    except RuntimeError as error:
        msg = f"mail-review-spam.py: {error}"
        raise SystemExit(msg) from error
