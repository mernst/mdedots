#!/usr/bin/env python3
"""Check a paper's reference list for hallucinated (fabricated) citations.

Usage:  python3 references-check.py refs.txt [output.txt]

The input file contains one reference per entry, each entry starting with a
bracketed number such as "[12]" at the beginning of a line.  Entries may wrap
across several lines, as happens when text is extracted from a PDF.

For each reference the script gathers independent evidence that the cited work
exists, and prints it so that a human (or an LLM) can compare the evidence
against what the reference claims:

  * DOI      -- resolved through Crossref, falling back to DataCite.  DataCite
                covers arXiv DOIs (10.48550/...), Dagstuhl, and Zenodo.
  * arXiv ID -- resolved through the arXiv Atom API.
  * title    -- looked up in DBLP, which indexes essentially all of computer
                science and is the best check for entries that carry no DOI
                (technical reports, "to appear" papers, USENIX papers).  DBLP
                is queried only when no DOI or arXiv lookup already confirmed
                the title, because DBLP throttles aggressively.
  * title    -- searched in OpenAlex when DBLP is unreachable or has no hit,
                then in Crossref, then in Semantic Scholar.  OpenAlex rations
                by daily quota and goes silent for hours once that quota is
                spent, so a long run needs the later search services to avoid
                reporting its tail as unchecked.  The four cover different
                material: Crossref's bibliographic search ranks journal
                articles first and buries book chapters, whereas Semantic
                Scholar indexes chapters and pre-1990 work.
  * GitHub   -- a github.com repository, issue, pull request, or discussion is
                resolved through the GitHub REST API rather than by scraping
                the HTML page, so that the script reports the item's real
                title, state, and creation date.  Software-engineering papers
                cite bug reports heavily, and a fabricated issue number is a
                404 from the API, which is unambiguous.
  * artifact  -- an anonymous.4open.science artifact is checked through that
                site's file-listing API.  Fetching the artifact's own URL
                proves nothing, because it redirects to an endpoint that
                answers 401 whether or not the artifact exists.
  * URL      -- any other URL is fetched, and the script reports the HTTP
                status and whether the reference's title occurs in the page.

A title taken from a PDF can contain a word that was hyphenated across a line
break and then joined, such as "Userperceived" for "User-perceived".  When a
title search finds nothing, the script retries with the word boundaries
restored, so that this extraction artifact does not masquerade as a fabricated
reference.

The script also compares the title claimed by the reference against the title
returned by each service, and labels the comparison MATCH, CLOSE, PARTIAL (one
title contains the other, but the extra material is not a subtitle, so the two
may name different works), PREFIX-ONLY (one title is a much shorter prefix of
the other), or MISMATCH.  A MISMATCH is
the strongest signal of a fabricated citation: it means the DOI or arXiv ID is
real but belongs to a different paper.  "NO HITS" from DBLP for a reference
that has no DOI is the second strongest signal.

A CLOSE title from a search service is weak evidence: a search asked for a
title it does not have answers with the nearest thing it does have, which can
be another chapter of another book or the first author's thesis.  So the script
keeps asking the remaining services for an exact title, and an entry that ends
the run with only CLOSE support is listed separately in the summary for a human
to verify rather than being counted as confirmed.

A service that refuses to answer is reported as UNREACHABLE and is never
counted as evidence.  When every service refuses, the entry is flagged RETRY
rather than NOTE, because the run learned nothing about it; re-run those
entries later instead of reading anything into the silence.  DBLP and OpenAlex
both rate-limit, so scanning a long reference list can exhaust them, and the
run's tail then consists of RETRY entries that say nothing about the paper.

A service that answers 429 with a Retry-After measured in hours (an exhausted
daily quota rather than a momentary burst) is abandoned rather than waited out;
see MAX_RETRY_AFTER.  Waiting stalls the run on a single reference and leaves a
half-written evidence file.

The script never decides by itself that a reference is hallucinated; judging
requires reading the evidence, because legitimate references can differ from
the indexed metadata (preprint versus published title, venue abbreviations,
author name transliterations, and so on).
"""

import difflib
import html
import http.client
import json
import os
import pathlib
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

MAILTO = "michael.ernst@gmail.com"
UA = f"ref-checker/1.0 (mailto:{MAILTO})"

# The errors that a fetch can fail with: a timeout, a DNS failure, or a reset
# connection (OSError and its subclass urllib.error.URLError), a malformed
# response (http.client.HTTPException), or a body that is not the JSON or the
# UTF-8 it claims to be (ValueError, the superclass of json.JSONDecodeError and
# of UnicodeDecodeError).
FETCH_ERRORS = (OSError, ValueError, http.client.HTTPException)

# A DOI is "10." followed by a registrant code and a suffix.  PDF text
# extraction often injects spaces and hyphens-plus-newlines into the suffix, so
# the pattern tolerates whitespace, which strip_doi() then removes.
DOI_RE = re.compile(r"\b(10\.\d{4,9}/[-._;()/:a-zA-Z0-9\s]+)")
ARXIV_RE = re.compile(r"arxiv\.org/(?:abs|pdf)/([0-9]{4}\.[0-9]{4,5})(?:v\d+)?", re.IGNORECASE)
URL_RE = re.compile(r"https?://[^\s,;)\]]+")
# github.com/OWNER/REPO, optionally followed by /issues/N, /pull/N, or
# /discussions/N.  "pulls" is accepted because that is the API's spelling and a
# reference occasionally uses it.
GITHUB_RE = re.compile(
    r"https?://(?:www\.)?github\.com/([^/\s]+)/([^/\s#?]+?)(?:\.git)?"
    r"(?:/(issues|pull|pulls|discussions)/(\d+))?/*(?:[#?]\S*)?$",
    re.IGNORECASE,
)
FOUROPEN_RE = re.compile(r"https?://anonymous\.4open\.science/r/([A-Za-z0-9_.-]+)", re.IGNORECASE)
# Reference titles are delimited by curly quotes in these files; straight
# quotes are accepted too.
TITLE_RE = re.compile(r"[“\"](.+?)[,.]?[”\"]", re.DOTALL)


def strip_doi(raw):
    """Remove whitespace that PDF extraction inserted into a DOI, and trailing junk.

    Returns:
        The DOI, without whitespace and without trailing punctuation.
    """
    doi = re.sub(r"\s+", "", raw)
    return doi.rstrip(".,;)")


def doi_variants(raw):
    """Return the DOIs that `raw` might be, most likely first.

    PDF text extraction sometimes drops an underscore from a DOI suffix and
    leaves a space in its place, so Springer's "10.1007/3-540-45937-5_13" is
    extracted as "10.1007/3-540-45937-5 13".  Simply deleting the space yields a
    DOI that resolves to nothing, which makes a real reference look fabricated,
    so a space is also tried as an underscore.  Deletion is tried first, because
    a space more often comes from a line break inside a suffix that has no
    underscore at all ("10.1109/ASE6 3991.2025.00119").

    Returns:
        The candidate DOIs, without duplicates.
    """
    variants = [strip_doi(raw)]
    raw = raw.strip()
    if re.search(r"\S\s+\S", raw):
        underscored = re.sub(r"\s+", "_", raw).rstrip(".,;)")
        if underscored not in variants:
            variants.append(underscored)
    return variants


# PDF text extraction joins a word that was hyphenated across a line break into
# a single token: "User-perceived" becomes "Userperceived", and "large-scale"
# becomes "largescale".  Title comparison is unaffected, because normalize()
# discards hyphens, but a title *search* for the joined token finds nothing, so
# a real reference looks unconfirmed.  A word list lets the script rebuild the
# word boundary and search again.  Without a word list the script simply skips
# the repair, and such a reference is reported as unconfirmed for a human to
# judge, exactly as before.
WORD_LIST_PATH = "/usr/share/dict/words"


def load_words():
    """Read the system word list.

    Returns:
        The lowercase words in WORD_LIST_PATH, or an empty set when that file
        cannot be read.
    """
    try:
        with pathlib.Path(WORD_LIST_PATH).open(encoding="utf-8", errors="ignore") as f:
            return {w.strip().lower() for w in f if w.strip().isalpha()}
    except OSError:
        return set()


WORDS = load_words()


def split_joined(token):
    """Split a run-on token such as "Userperceived" into "User perceived".

    Returns:
        The token with the word boundary restored, or None when the token is a
        word in its own right, is too short to be a join, or has no split point
        that yields two words.
    """
    bare = token.strip(",.:;()[]'’“”")  # ruff:ignore[ambiguous-unicode-character-string]  curly quotes occur in the input
    low = bare.lower()
    if len(low) < 8 or not low.isalpha() or low in WORDS:
        return None
    for i in range(3, len(low) - 2):
        if low[:i] in WORDS and low[i:] in WORDS:
            return token.replace(bare, bare[:i] + " " + bare[i:])
    return None


def query_variants(title):
    """Return the search queries to try for a title, the most faithful first.

    The second query, present only when a run-on token was found, restores the
    word boundaries that PDF de-hyphenation destroyed.

    Returns:
        The queries to try, or an empty list when there is no title.
    """
    if not title:
        return []
    tokens = title.split()
    repaired = [split_joined(t) or t for t in tokens]
    if repaired == tokens:
        return [title]
    return [title, " ".join(repaired)]


def normalize(text):
    """Reduce a title to lowercase alphanumerics, for tolerant comparison.

    Returns:
        The text, reduced to its lowercase letters and digits.
    """
    return re.sub(r"[^a-z0-9]+", "", (text or "").lower())


# Title-comparison verdicts that are only inexact agreement.  Each one leaves a
# reference "confirmed" but flagged, because a search asked for a title it does
# not have answers with the nearest title it does have.
WEAK = ("CLOSE", "PARTIAL")

# Title-comparison verdicts that make a further DBLP query unnecessary.
CONFIRMING = ("MATCH", *WEAK)

# Punctuation that introduces a subtitle.  Material after one of these is a
# subtitle that a citation may legitimately drop; material appended without any
# punctuation names a different work.
SUBTITLE_DELIMITERS = ":?!-–—([{,;/"  # ruff:ignore[ambiguous-unicode-character-string]  a dash of each length is meant


def raw_split_point(raw, kept_chars):
    """Return the index just past raw's first kept_chars normalizable characters.

    Returns:
        The index in `raw` just past its kept_chars'th normalizable character, or
        the length of `raw` when it has fewer than that many.
    """
    kept = 0
    for i, ch in enumerate(raw):
        if normalize(ch):
            kept += 1
            if kept == kept_chars:
                return i + 1
    return len(raw)


def compare_titles(claimed, found):
    """Describe how well the claimed title and the found title agree.

    Returns:
        MATCH, CLOSE, PARTIAL, PREFIX-ONLY, MISMATCH, or "" when either title is
        empty.
    """
    a, b = normalize(claimed), normalize(found)
    if not a or not b:
        return ""
    if a == b:
        return "MATCH"
    if a in b or b in a:
        # Registries sometimes store a truncated title, and a reference
        # sometimes drops a subtitle.  Containment is weak evidence when one
        # title is much shorter, so say so rather than claiming a full match.
        shorter, longer = sorted((len(a), len(b)))
        if shorter <= 0.6 * longer:
            return "PREFIX-ONLY"
        # Containment alone does not establish identity, because appending a
        # word to a title names a different work: "Enforceable Security
        # Policies" and "Enforceable Security Policies Revisited" are two
        # papers by different authors.  Count the extra material as a dropped
        # subtitle only when punctuation introduces it.
        long_raw, short_norm = (found, a) if len(b) > len(a) else (claimed, b)
        if normalize(long_raw).startswith(short_norm):
            tail = long_raw[raw_split_point(long_raw, len(short_norm)) :].lstrip()
            if tail[:1] in SUBTITLE_DELIMITERS:
                return "MATCH"
        return "PARTIAL"
    ratio = difflib.SequenceMatcher(None, a, b).ratio()
    return "CLOSE" if ratio > 0.75 else "MISMATCH"


# The longest Retry-After the script will actually wait out.  A service that
# rations by daily quota answers an exhausted quota with the seconds remaining
# until the quota resets: OpenAlex returns "Retry-After: 36978", ten hours.
# Sleeping for that stalls the whole run on one reference and leaves the
# evidence file truncated, which looks indistinguishable from a crash.  Beyond
# this cap the script gives up on the service at once and reports it
# UNREACHABLE, which the caller already refuses to read as evidence.
MAX_RETRY_AFTER = 30.0


def fetch(url, tries=4, accept_json=True, delay=3.0):
    """Fetch a URL, retrying transient failures.

    DBLP throttles by resetting the connection rather than by returning 429, so
    connection errors are retried with an exponentially growing delay.

    Returns:
        The HTTP status and the body, the body being the parsed JSON when
        `accept_json`.  The status is None when the fetch never completed, and
        the body is None when the response carried no usable one.
    """
    for attempt in range(tries):
        req = urllib.request.Request(url, headers={"User-Agent": UA})
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                raw = resp.read()
                if accept_json:
                    return 200, json.loads(raw)
                return resp.status, raw.decode("utf-8", "replace")
        except urllib.error.HTTPError as exc:
            if exc.code in (404, 403, 410):
                return exc.code, None
            if attempt == tries - 1:
                return exc.code, None
            if exc.code == 429:
                # Honor the service's own pacing request, and wait at least a
                # few seconds, because a burst of retries only prolongs the ban.
                retry_after = exc.headers.get("Retry-After", "") if exc.headers else ""
                requested = float(retry_after) if retry_after.isdigit() else 10.0
                if requested > MAX_RETRY_AFTER:
                    return exc.code, None
                delay = max(delay, requested)
        except FETCH_ERRORS as exc:
            if attempt == tries - 1:
                return None, str(exc)
        time.sleep(delay)
        delay = min(delay * 1.5, 30.0)
    return None, "unreachable"


def parse_refs(path):
    """Split a reference file into its entries.

    Returns:
        The references, each one a pair of its number and its text.
    """
    lines = pathlib.Path(path).read_text(encoding="utf-8").splitlines()
    refs, num, buf = [], None, []
    for line in lines:
        m = re.match(r"\s*\[(\d+)\]\s*(.*)", line)
        if m:
            if num is not None:
                refs.append((num, " ".join(buf)))
            num, buf = int(m.group(1)), [m.group(2)]
        elif num is not None:
            buf.append(line.strip())
    if num is not None:
        refs.append((num, " ".join(buf)))
    return refs


# An author list, as a run of "A." / "A. B." initials followed by a surname,
# separated by commas and "and".  Used only to find where a book's title starts.
AUTHORS_RE = re.compile(
    r"^\s*(?:(?:[A-Z]\.\s*){1,3}[A-ZÅÄÖÉ][\w'’.-]+(?:\s+[A-Z][\w'’-]+)?"  # ruff:ignore[ambiguous-unicode-character-string]
    r"(?:\s*,\s*|\s+and\s+|\s*,?\s*and\s+)?)+,\s*"
)
# A period that ends a sentence, rather than one that follows an initial or an
# abbreviation such as "vol." or "Inc.".
SENTENCE_END_RE = re.compile(r"(?<![A-Z])\.\s+(?=[A-Z0-9])")


def claimed_title(text):
    """Return the title the reference claims, and whether it was quoted.

    A paper's title is delimited by quotation marks, but a book's is not: a book
    reference reads "L. B. Rall, Automatic differentiation: Techniques and
    applications. Springer, 1981."  Taking no title for those left every book in
    a reference list unchecked, which is exactly backwards -- a fabricated book
    is as likely as a fabricated paper.  So fall back to the text between the
    author list and the publisher.  The fallback is a heuristic, so the caller
    reports it as such and a MISMATCH against it means less than a MISMATCH
    against a quoted title.

    Returns:
        The title, and whether quotation marks delimited it.  The title is "" if
        neither the quotation marks nor the fallback yielded one.
    """
    m = TITLE_RE.search(text)
    if m:
        return m.group(1).strip(), True
    rest = AUTHORS_RE.sub("", text)
    if rest == text:
        return "", False
    end = SENTENCE_END_RE.search(rest)
    guess = (rest[: end.start()] if end else rest).strip().rstrip(".,")
    # A plausible title has several words; anything shorter is parsing debris.
    return (guess, False) if len(guess.split()) >= 3 else ("", False)


def check_crossref(doi, title, out):
    """Report what Crossref says about the DOI.

    Returns:
        The verdict of comparing `title` against the title Crossref holds, or ""
        when Crossref does not know the DOI.
    """
    status, data = fetch(f"https://api.crossref.org/works/{urllib.parse.quote(doi)}")
    if status != 200 or not isinstance(data, dict) or "message" not in data:
        emit(out, "CROSSREF", f"not found ({status})")
        return ""
    msg = data["message"]
    found = "; ".join(msg.get("title", []))
    verdict = compare_titles(title, found)
    authors = ", ".join(
        ("{} {}".format(a.get("given", ""), a.get("family", ""))).strip()
        for a in msg.get("author", [])
    )
    date = msg.get("issued", {}).get("date-parts", [[None]])[0]
    venue = "; ".join(msg.get("container-title", []) or [])
    emit(out, "CROSSREF", f"{found}  [{verdict}]")
    emit(out, "", f"authors: {authors}")
    emit(
        out,
        "",
        f"venue: {venue} | vol {msg.get('volume')} no {msg.get('issue')} "
        f"| pp {msg.get('page')} | {date}",
    )
    return verdict


def check_datacite(doi, title, out):
    """Report what DataCite says about the DOI.

    Returns:
        The verdict of comparing `title` against the title DataCite holds, or ""
        when DataCite does not know the DOI.
    """
    url = f"https://api.datacite.org/dois/{urllib.parse.quote(doi, safe='')}"
    status, data = fetch(url)
    if status != 200 or not isinstance(data, dict) or "data" not in data:
        emit(out, "DATACITE", f"not found ({status})")
        return ""
    at = data["data"]["attributes"]
    titles = at.get("titles") or [{}]
    found = titles[0].get("title", "")
    verdict = compare_titles(title, found)
    emit(out, "DATACITE", f"{found}  [{verdict}]")
    creators = ", ".join(c.get("name", "") for c in at.get("creators", []))
    emit(out, "", f"authors: {creators}")
    emit(out, "", "year: {} | publisher: {}".format(at.get("publicationYear"), at.get("publisher")))
    return verdict


def arxiv_entries(body):
    """Parse an arXiv Atom response.

    Returns:
        The entries of the response, each one a tuple of its title, its authors,
        its date, and its arXiv identifier.
    """
    entries = []
    for entry in re.findall(r"<entry>(.*?)</entry>", body, re.DOTALL):

        def tag(name, entry=entry):
            m = re.search(rf"<{name}>(.*?)</{name}>", entry, re.DOTALL)
            return html.unescape(re.sub(r"\s+", " ", m.group(1)).strip()) if m else ""

        authors = [html.unescape(a) for a in re.findall(r"<name>(.*?)</name>", entry)]
        entries.append((tag("title"), authors, tag("published"), tag("id")))
    return entries


def check_arxiv(arxiv_id, title, out):
    """Report what the arXiv API says about the arXiv identifier.

    Returns:
        The verdict of comparing `title` against the title arXiv holds, or ""
        when arXiv does not know the identifier.
    """
    url = f"http://export.arxiv.org/api/query?id_list={arxiv_id}"
    status, body = fetch(url, accept_json=False)
    if status != 200 or not isinstance(body, str):
        emit(out, "ARXIV", f"not found ({status})")
        return ""
    entries = arxiv_entries(body)
    if not entries:
        emit(out, "ARXIV", "NO SUCH PAPER")
        return ""
    found, authors, date, _ = entries[0]
    verdict = compare_titles(title, found)
    emit(out, "ARXIV", f"{found}  [{verdict}]")
    emit(out, "", f"authors: {', '.join(authors)}")
    emit(out, "", f"date: {date}")
    return verdict


def check_dblp(title, out):
    """Search DBLP for the title, repairing run-on words if the first query fails.

    Returns:
        Whether DBLP answered, and the best title verdict among its hits.
    """
    answered, best = True, ""
    for query in query_variants(title):
        answered, best = dblp_query(query, title, out)
        if best in CONFIRMING or not answered:
            break
    return answered, best


def dblp_query(query, title, out):
    """Search DBLP for one query string, comparing each hit against `title`.

    Returns:
        Whether DBLP answered, and the best title verdict among its hits.
    """
    url = f"https://dblp.org/search/publ/api?q={urllib.parse.quote(query)}&format=json&h=4"
    status, data = fetch(url, tries=3, delay=10.0)
    if status != 200 or not isinstance(data, dict):
        # DBLP blocks an address that has queried too often, by resetting the
        # connection.  That says nothing about the reference, so fall back.
        emit(out, "DBLP", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False, ""
    hits = data.get("result", {}).get("hits", {}).get("hit", [])
    if not hits:
        emit(out, "DBLP", f"NO HITS for {query!r}")
        return True, ""
    best = ""
    for n, h in enumerate(hits):
        i = h["info"]
        au = i.get("authors", {}).get("author", [])
        if isinstance(au, dict):
            au = [au]
        names = ", ".join(a["text"] for a in au)
        found = i.get("title", "")
        verdict = compare_titles(title, found)
        best = best or (verdict if verdict in CONFIRMING else "")
        emit(out, "DBLP" if n == 0 else "", f"{found}  [{verdict}]")
        emit(out, "", "  {} | {} {}".format(names, i.get("venue"), i.get("year")))
    return True, best


def check_openalex(title, out):
    """Search OpenAlex by title.  Used when DBLP is unreachable or finds nothing.

    OpenAlex covers all disciplines and does not throttle, but its title search
    is fuzzy, so every hit's title is compared and labeled.

    Returns:
        Whether OpenAlex answered, and the best title verdict among its hits.
    """
    answered, best = True, ""
    for query in query_variants(title):
        answered, best = openalex_query(query, title, out)
        if best in CONFIRMING or not answered:
            break
    return answered, best


def openalex_query(query, title, out):
    """Search OpenAlex for one query string, comparing each hit against `title`.

    Returns:
        Whether OpenAlex answered, and the best title verdict among its hits.
    """
    url = f"https://api.openalex.org/works?filter=title.search:{urllib.parse.quote(query)}&per-page=4&mailto={urllib.parse.quote(MAILTO)}"
    status, data = fetch(url)
    if status != 200 or not isinstance(data, dict):
        # A refusal (429) or an outage says nothing about the reference.
        emit(out, "OPENALEX", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False, ""
    results = data.get("results", [])
    if not results:
        emit(out, "OPENALEX", f"NO HITS for {query!r}")
        return True, ""
    best = ""
    for n, w in enumerate(results):
        found = w.get("title") or ""
        names = ", ".join(a["author"]["display_name"] for a in w.get("authorships", []))
        loc = w.get("primary_location") or {}
        venue = (loc.get("source") or {}).get("display_name")
        verdict = compare_titles(title, found)
        best = best or (verdict if verdict in CONFIRMING else "")
        emit(out, "OPENALEX" if n == 0 else "", f"{found}  [{verdict}]")
        emit(
            out,
            "",
            "  {} | {} {} | {}".format(names, venue, w.get("publication_year"), w.get("doi") or ""),
        )
    return True, best


def check_crossref_search(title, out):
    """Search Crossref by title.  Used when DBLP and OpenAlex both come up empty.

    Crossref indexes the publishers that matter here (ACM, IEEE, Springer,
    Elsevier) and, unlike OpenAlex, does not ration by daily quota, so it still
    answers after a long run has exhausted the other services.  It does not
    cover arXiv preprints or technical reports, so it supplements rather than
    replaces them.

    Returns:
        Whether Crossref answered, and the best title verdict among its hits.
    """
    answered, best = True, ""
    for query in query_variants(title):
        answered, best = crossref_search_query(query, title, out)
        if best in CONFIRMING or not answered:
            break
    return answered, best


def crossref_search_query(query, title, out):
    """Search Crossref for one query string, comparing each hit against `title`.

    Returns:
        Whether Crossref answered, and the best title verdict among its hits.
    """
    url = f"https://api.crossref.org/works?query.bibliographic={urllib.parse.quote(query)}&rows=4&mailto={urllib.parse.quote(MAILTO)}"
    status, data = fetch(url, tries=3)
    if status != 200 or not isinstance(data, dict):
        emit(out, "CR-SEARCH", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False, ""
    items = data.get("message", {}).get("items", [])
    if not items:
        emit(out, "CR-SEARCH", f"NO HITS for {query!r}")
        return True, ""
    best = ""
    for n, it in enumerate(items):
        found = "; ".join(it.get("title", []) or [])
        verdict = compare_titles(title, found)
        names = ", ".join(
            ("{} {}".format(a.get("given", ""), a.get("family", ""))).strip()
            for a in it.get("author", [])
        )
        venue = "; ".join(it.get("container-title", []) or [])
        year = it.get("issued", {}).get("date-parts", [[None]])[0][0]
        emit(out, "CR-SEARCH" if n == 0 else "", f"{found}  [{verdict}]")
        emit(out, "", "  {} | {} {} | {}".format(names, venue, year, it.get("DOI", "")))
        if verdict not in CONFIRMING:
            # Crossref indexes a book one chapter at a time, so a cited book
            # surfaces as some chapter of itself, whose container-title is the
            # book.  Matching the container confirms the book exists, which is
            # the question being asked, even though the hit's own title differs.
            container = compare_titles(title, venue)
            if container in CONFIRMING or container == "PREFIX-ONLY":
                # PREFIX-ONLY is common and meaningful here: a publisher often
                # registers a book under its main title alone, while the
                # reference gives the subtitle too.
                emit(
                    out,
                    "",
                    "  ^ container title agrees with the cited work "
                    f"[{container}]; the hit is a chapter of it",
                )
                verdict = container
        best = best or (verdict if verdict in CONFIRMING else "")
    return True, best


def check_semantic_scholar(title, out):
    """Search Semantic Scholar by title.  The last resort when the others fail.

    Semantic Scholar covers material the earlier services miss: book chapters,
    technical reports, and pre-1990 work, which Crossref's bibliographic search
    ranks below any journal article with similar words.  Its quota is separate
    from DBLP's and OpenAlex's, so it still answers on a day when those are
    spent.  Its metadata is the noisiest of the four -- it dates a scanned
    reprint by the scan -- so a hit is evidence that the work exists, not a
    correction to the reference's year or venue.

    Returns:
        Whether Semantic Scholar answered, and the best title verdict among its
        hits.
    """
    answered, best = True, ""
    for query in query_variants(title):
        answered, best = semantic_scholar_query(query, title, out)
        if best in CONFIRMING or not answered:
            break
    return answered, best


def semantic_scholar_query(query, title, out):
    """Search Semantic Scholar for one query string, comparing each hit's title.

    Returns:
        Whether Semantic Scholar answered, and the best title verdict among its
        hits.
    """
    url = (
        "https://api.semanticscholar.org/graph/v1/paper/search/match"
        f"?query={urllib.parse.quote(query)}&fields=title,authors,year,venue,externalIds"
    )
    status, data = fetch(url, tries=3)
    if status == 404:
        # The match endpoint answers 404 when no indexed title is close enough.
        # That is an answer about the reference, not a refusal to answer.
        emit(out, "S2", f"NO HITS for {query!r}")
        return True, ""
    if status != 200 or not isinstance(data, dict):
        emit(out, "S2", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False, ""
    hits = data.get("data", [])
    if not hits:
        emit(out, "S2", f"NO HITS for {query!r}")
        return True, ""
    best = ""
    for n, w in enumerate(hits):
        found = w.get("title") or ""
        names = ", ".join(a.get("name", "") for a in w.get("authors", []))
        verdict = compare_titles(title, found)
        best = best or (verdict if verdict in CONFIRMING else "")
        doi = (w.get("externalIds") or {}).get("DOI") or ""
        emit(out, "S2" if n == 0 else "", f"{found}  [{verdict}]")
        emit(out, "", f"  {names} | {w.get('venue') or ''} {w.get('year')} | {doi}")
    return True, best


def check_arxiv_search(title, out):
    """Search arXiv by title, for a reference that cites a preprint.

    A preprint frequently changes its name on the way to publication, and the
    registries then index only the published title.  A reference that cites the
    preprint therefore comes back CLOSE or NO HITS from every other service,
    which reads like a fabrication; arXiv still holds the original title.  This
    runs last because arXiv indexes only preprints and paces requests strictly.

    Returns:
        Whether arXiv answered, and the best title verdict among its hits.
    """
    answered, best = True, ""
    for query in query_variants(title):
        answered, best = arxiv_search_query(query, title, out)
        if best in CONFIRMING or not answered:
            break
    return answered, best


def arxiv_search_query(query, title, out):
    """Search arXiv for one query string, comparing each hit against `title`.

    Returns:
        Whether arXiv answered, and the best title verdict among its hits.
    """
    # Search "all:" rather than "ti:".  A citation's punctuation rarely matches
    # the preprint's ("Fix-Con" versus "FixCon"), and arXiv's title index is
    # literal enough to miss on that, whereas "all:" also reaches the abstract.
    # Long phrases return nothing at all, so use only the leading words.
    words = re.findall(r"[A-Za-z0-9]+", query)[:12]
    if not words:
        return True, ""
    phrase = urllib.parse.quote(" ".join(words))
    url = f"http://export.arxiv.org/api/query?search_query=all:%22{phrase}%22&max_results=4"
    status, body = fetch(url, tries=3, accept_json=False, delay=5.0)
    if status != 200 or not isinstance(body, str):
        emit(out, "ARXIV-S", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False, ""
    entries = arxiv_entries(body)
    if not entries:
        emit(out, "ARXIV-S", f"NO HITS for {query!r}")
        return True, ""
    best = ""
    for n, (found, authors, date, ident) in enumerate(entries):
        verdict = compare_titles(title, found)
        best = best or (verdict if verdict in CONFIRMING else "")
        emit(out, "ARXIV-S" if n == 0 else "", f"{found}  [{verdict}]")
        emit(out, "", "  {} | {} | {}".format(", ".join(authors), date[:10], ident))
    return True, best


# The title searches, in the order the script tries them.  Each takes (title,
# out) and returns (did-the-service-answer, best-verdict).
TITLE_SEARCHES = (
    check_dblp,
    check_openalex,
    check_crossref_search,
    check_semantic_scholar,
    check_arxiv_search,
)


# GitHub allows 60 unauthenticated API requests per hour per address, which one
# reference list full of bug reports can exhaust.  A token in the environment
# raises that to 5000; without one the script still works, and reports a
# throttled request as UNREACHABLE rather than as a missing issue.
GITHUB_TOKEN = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")


def github_api(path):
    """Fetch a GitHub API path.

    Returns:
        The HTTP status and the parsed JSON, either of which is None when the
        request did not succeed.
    """
    url = "https://api.github.com/" + path
    headers = {"User-Agent": UA, "Accept": "application/vnd.github+json"}
    if GITHUB_TOKEN:
        headers["Authorization"] = f"Bearer {GITHUB_TOKEN}"
    for attempt in range(3):
        try:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=30) as resp:
                return 200, json.loads(resp.read())
        except urllib.error.HTTPError as exc:
            if exc.code == 404:
                return 404, None
            if attempt == 2:
                return exc.code, None
        except FETCH_ERRORS:
            if attempt == 2:
                return None, None
        time.sleep(5)
    return None, None


def check_github(match, title, out):
    """Report what the GitHub API says about a repository, issue, or pull request.

    A bug report has no DOI and is not indexed anywhere, so the API is the only
    check available for it.

    Returns:
        True when the API confirmed the cited item, which is evidence that the
        reference is real even when no bibliographic registry knows it.
    """
    owner, repo, kind, number = match.group(1), match.group(2), match.group(3), match.group(4)
    if not kind:
        status, data = github_api(f"repos/{owner}/{repo}")
        if status == 404:
            emit(out, "GITHUB", f"NO SUCH REPOSITORY {owner}/{repo}")
            return False
        if status != 200 or not isinstance(data, dict):
            emit(out, "GITHUB", f"UNREACHABLE ({status}) -- tool failure, not evidence")
            return False
        # The API answers under a repository's old name too, and reports the
        # current one, so a renamed or transferred project is not a mismatch.
        emit(out, "GITHUB", f"repo {data.get('full_name')}  (cited as {owner}/{repo})")
        emit(out, "", f"description: {data.get('description') or ''}")
        emit(out, "", f"created: {data.get('created_at')} | stars: {data.get('stargazers_count')}")
        return True

    # One API path serves issues and pull requests alike; a discussion needs a
    # different one, and the API does not expose discussions without GraphQL,
    # so fall back to fetching the page for those.
    if kind == "discussions":
        return False
    status, data = github_api(f"repos/{owner}/{repo}/issues/{number}")
    label = "issue" if kind == "issues" else "PR"
    if status == 404:
        emit(out, "GITHUB", f"NO SUCH {label.upper()} {owner}/{repo}#{number}")
        return False
    if status != 200 or not isinstance(data, dict):
        emit(out, "GITHUB", f"UNREACHABLE ({status}) -- tool failure, not evidence")
        return False
    found = data.get("title", "")
    verdict = compare_titles(title, found)
    is_pr = "pull_request" in data
    opener = (data.get("user") or {}).get("login")
    emit(out, "GITHUB", f"{owner}/{repo}#{number} {found}  [{verdict}]")
    emit(
        out,
        "",
        f"kind: {'PR' if is_pr else 'issue'} (cited as {label}) | state: {data.get('state')} "
        f"| opened: {data.get('created_at')} by {opener}",
    )
    if is_pr != (kind != "issues"):
        # Citing a pull request as an issue, or the reverse, is a sloppy
        # citation rather than a fabricated one; say so without hiding it.
        emit(
            out,
            "",
            f"  note: cited under /{kind}/ but GitHub calls it a "
            f"{'pull request' if is_pr else 'issue'}",
        )
    return verdict in CONFIRMING


def check_4open(artifact_id, out):
    """Report whether an anonymous.4open.science artifact exists.

    The artifact's browser URL redirects to an endpoint that answers 401 for
    an existing artifact and for a fabricated one alike, so ask the file
    listing instead: it names the artifact's top-level files when the artifact
    is real, and fails when it is not.

    Returns:
        True when the file listing shows that the artifact exists.
    """
    url = f"https://anonymous.4open.science/api/repo/{urllib.parse.quote(artifact_id)}/files/"
    status, data = fetch(url, tries=2)
    if status != 200 or not isinstance(data, list) or not data:
        emit(out, "4OPEN", f"artifact {artifact_id} NOT FOUND ({status})")
        return False
    names = ", ".join(str(e.get("name", "")) for e in data[:12])
    emit(out, "4OPEN", f"artifact {artifact_id} exists, {len(data)} top-level entries")
    emit(out, "", f"contents: {names}")
    return True


def check_url(url, title, out):
    """Fetch a URL, and report its status and whether the page mentions the title."""
    status, body = fetch(url, tries=2, accept_json=False)
    if status != 200 or not isinstance(body, str):
        emit(out, "URL", f"{url} -> {status}")
        return
    # Discard URLs in the page before looking for the title.  Many sites build
    # a page's own address out of its title ("...  /Rusted-Types-Static-..."),
    # so a title found inside a URL is no evidence that the page is about the
    # cited work; the address came from the reference being checked.
    body = URL_RE.sub(" ", body)
    body = re.sub(r"""(?:href|src|content)\s*=\s*["'][^"']*["']""", " ", body)
    text = normalize(re.sub(r"<[^>]+>", " ", body))
    present = (
        "title present in page text"
        if normalize(title) and normalize(title) in text
        else "TITLE NOT IN PAGE TEXT"
    )
    emit(out, "URL", f"{url} -> 200, {present}")


def emit(out, label, text):
    """Write one line of evidence, under the name of the service that supplied it."""
    print(f"    {label:<9} {text}", file=out)


def check_reference(num, text, out):
    """Gather and write every piece of evidence about one reference.

    Returns:
        OK, CLOSE, NOTE, or RETRY, summarizing what the evidence amounts to.
    """
    title, quoted = claimed_title(text)
    print(f"=== [{num}] {title or text[:90]}", file=out)
    print(f"    {text}", file=out)
    if title and not quoted:
        emit(out, "TITLE?", f"no quoted title; searching for {title!r} (heuristic)")
    elif not title:
        emit(out, "TITLE?", "could not extract a title; judge from the URLs below")

    seen, resolved = set(), False
    for raw in DOI_RE.findall(text):
        for doi in doi_variants(raw):
            if doi in seen:
                continue
            seen.add(doi)
            verdict = check_crossref(doi, title, out) or check_datacite(doi, title, out)
            if verdict in CONFIRMING:
                resolved = True
                # The remaining variants are misreadings of this same DOI.
                break

    for arxiv_id in set(ARXIV_RE.findall(text)):
        if check_arxiv(arxiv_id, title, out) in CONFIRMING:
            resolved = True

    # Resolve GitHub and artifact URLs before searching for the title.  A bug
    # report or a research artifact is indexed by no bibliographic registry, so
    # searching for its title only burns the rate limits that later references
    # need, and reports a NO HITS that means nothing.
    hosted = []  # URLs already accounted for by a hosting-site check
    for url in URL_RE.findall(text):
        url = url.rstrip(".,")
        gh = GITHUB_RE.match(url)
        fo = FOUROPEN_RE.match(url)
        if gh:
            hosted.append(url)
            if check_github(gh, title, out):
                resolved = True
        elif fo:
            hosted.append(url)
            if check_4open(fo.group(1), out):
                resolved = True

    # DBLP is the only check available for a reference with no DOI, so consult
    # it whenever the registries did not already confirm the title.  Skipping
    # it for confirmed references keeps the run fast, because DBLP throttles
    # aggressively and each throttled query costs a minute of backoff.
    searched = False  # did any title-search service actually answer?
    weak = False  # confirmed only by an inexact title, never by an exact one
    if not resolved:
        for search_service in TITLE_SEARCHES:
            answered, verdict = search_service(title, out)
            searched = searched or answered
            if verdict == "MATCH":
                resolved, weak = True, False
                break
            if verdict in WEAK:
                # An inexact hit from a title *search* is frequently a different
                # work with a similar name: another chapter of another book, the
                # first author's thesis, or a later paper that appends a word to
                # the title it answers.  Accept it, but keep asking the remaining
                # services for an exact title, and say so below if none supplies
                # one.
                resolved, weak = True, True

    # Fetch any remaining URL (USENIX, conf.researchr.org, project pages) that
    # was not already covered by a DOI, arXiv, or hosting-site lookup.
    for url in URL_RE.findall(text):
        url = url.rstrip(".,")
        if "doi.org" in url or "arxiv.org" in url or url in hosted:
            continue
        check_url(url, title, out)

    if not resolved and not searched and not seen and not hosted:
        # Every service refused to answer, so the run produced no evidence at
        # all about this reference.  Saying "not confirmed" here would invite
        # the reader to suspect a reference that was never actually checked.
        emit(out, "RETRY", "INCONCLUSIVE -- no service answered; re-run this entry later")
        status = "RETRY"
    elif not resolved:
        emit(out, "NOTE", "NOT CONFIRMED by any registry; judge from the evidence above")
        status = "NOTE"
    elif weak:
        emit(out, "CLOSE", "supported only by an inexact title, never an exact one; verify by hand")
        status = "CLOSE"
    else:
        status = "OK"
    print(file=out)
    out.flush()
    return status


def numbers(nums):
    """Format reference numbers as a list of bracketed labels.

    Returns:
        The numbers, each in brackets, separated by commas.
    """
    return ", ".join(f"[{n}]" for n in nums)


def summarize(results, out):
    """List the entries a human still has to judge, so nobody has to grep for them.

    A long run confirms most references and leaves a handful open, and those few
    are the whole point.  Printing them together also makes it obvious when a
    run was ruined by throttling: a tail of RETRY entries means the services
    stopped answering, not that the paper's citations went bad two thirds of the
    way down the list.
    """
    notes = [n for n, s in results if s == "NOTE"]
    retries = [n for n, s in results if s == "RETRY"]
    closes = [n for n, s in results if s == "CLOSE"]
    confirmed = len(results) - len(notes) - len(retries) - len(closes)
    print("=" * 70, file=out)
    print(f"{confirmed} of {len(results)} references confirmed automatically.", file=out)
    if notes:
        print(
            f"NOT CONFIRMED -- read the evidence and judge: {numbers(notes)}",
            file=out,
        )
    if closes:
        print(
            f"CLOSE TITLE ONLY -- no service returned the exact title; verify: {numbers(closes)}",
            file=out,
        )
    if retries:
        print(
            f"NEVER CHECKED -- every service refused; re-run these: {numbers(retries)}",
            file=out,
        )
    if not notes and not retries and not closes:
        print("Every reference was confirmed by at least one service.", file=out)


def check_all(refs, source, out):
    """Check every reference of `source`, writing the evidence and the summary to `out`."""
    print(f"{len(refs)} references in {source}\n", file=out)
    results = []
    for num, text in refs:
        results.append((num, check_reference(num, text, out)))
        time.sleep(1)
    summarize(results, out)


def main():
    """Check the references in the file that is named on the command line."""
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    refs = parse_refs(sys.argv[1])
    if len(sys.argv) > 2:
        with pathlib.Path(sys.argv[2]).open("w", encoding="utf-8") as out:
            check_all(refs, sys.argv[1], out)
    else:
        check_all(refs, sys.argv[1], sys.stdout)


# Guarded so that the checks can be imported and exercised one at a time, which
# is how a single suspect reference is re-examined without spending the rate
# limits that a whole-file run needs.
if __name__ == "__main__":
    main()
