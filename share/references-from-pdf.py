#!/usr/bin/env python3

"""Extract a paper's bibliography from a PDF file, into a text file.

Given the file name of a paper in PDF format, write a text file that contains the
paper's reference list, one reference per line.

The script locates the "References" heading and the end of the reference list by
analyzing the text layout that "pdftotext -bbox-layout" produces.  It reads the
reference list in reading order (down each column of each page), discards running
heads, page numbers, and the line numbers of a submission, groups the lines into
references, and joins the lines of each reference.

A line is recognized as the first line of a reference by its label ("[12]" or "3."),
or by its indentation, or by the shortness of the line that precedes it.

Requires the program pdftotext, which is part of poppler.
"""

import argparse
import collections
import itertools
import pathlib
import re
import shutil
import statistics
import subprocess
import sys
import textwrap
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from typing import NoReturn

# The XHTML namespace, which "pdftotext -bbox-layout" uses for its output.
XHTML = "{http://www.w3.org/1999/xhtml}"

# Titles of a reference list, with all spaces and punctuation removed.
REFERENCES_TITLES = frozenset(
    [
        "references",
        "referencesandnotes",
        "referencescited",
        "bibliography",
        "literaturecited",
        "workscited",
        "worksconsulted",
    ]
)

# Text (lowercased, with the section number removed) of a heading that indicates
# that the reference list has ended.
END_HEADING_RE = re.compile(
    r"""^(
          appendix\b.*
        | appendices
        | supplement(al|ary)?(\s+(material|information))?
        | (author\s+)?(biograph(y|ies)|bios?)
        | about\s+the\s+authors?
        | acknowledg(e)?ments?
        | vitae?
        | received\b.*\baccepted\b.*
        )$""",
    re.VERBOSE,
)

# A section number at the beginning of a heading:  "5", "5.2.", "A", "IV.".
SECTION_NUMBER_RE = re.compile(r"^(?:[0-9]+(?:\.[0-9]+)*|[A-Z]|[IVX]+)\.?\s+")

# A word that is set in small capitals.  pdftotext writes a space after the initial
# letter of such a word, as in "R EFERENCES" and "VI. R ELATED W ORK".
SMALL_CAPS_WORD_RE = re.compile(r"\b([A-Z]) ([A-Z]{2,})\b")

# A character that XML 1.0 forbids.  The output of pdftotext contains such a
# character for a glyph that has no Unicode representation.
INVALID_XML_CHARACTER_RE = re.compile(r"[\x00-\x08\x0b\x0c\x0e-\x1f]")

# The label at the beginning of a reference, in brackets or parentheses:  "[12]",
# "[Ern03]", "(12)".  A continuation line hardly ever begins with such a label.
BRACKETED_LABEL_RE = re.compile(
    r"^(?:\[\s*[0-9A-Za-z][^\[\]]{0,20}\]|\(\s*[0-9]{1,3}\s*\))(?=\s|$)"
)

# The label at the beginning of a reference, as a bare number:  "12.".  A continuation
# line often begins this way too, with a year or the end of a range of page numbers.
NUMERIC_LABEL_RE = re.compile(r"^([0-9]{1,3})\.(?=\s|$)")

# Any label at the beginning of a reference, in either style.
LABEL_RE = re.compile(BRACKETED_LABEL_RE.pattern + "|" + NUMERIC_LABEL_RE.pattern)

# A line that consists of nothing but digits, such as a line number in the margin.
DIGITS_ONLY_RE = re.compile(r"^[0-9]{1,5}$")

# How many numbers must stand in a vertical row in the margin before they are treated
# as the line numbers of a submission rather than as text.
MIN_LINE_NUMBERS = 5

# Ligatures that some PDF files use in place of separate characters.
LIGATURES = {
    "ﬀ": "ff",
    "ﬁ": "fi",
    "ﬂ": "fl",
    "ﬃ": "ffi",
    "ﬄ": "ffl",
    "ﬅ": "st",
    "ﬆ": "st",
}

# How much farther right than the left margin of a column a line must start, in
# points, for the line to count as indented.
INDENT_TOLERANCE = 1.5

# How far right of the left margin of a column, in points, a heading may start.  This
# is smaller than the indentation of the continuation lines of a reference.
HEADING_MARGIN_TOLERANCE = 6.0

# The fraction of the height of a page that is treated as its top or bottom margin.
MARGIN_FRACTION = 0.15


@dataclass
class Line:
    """One line of text on a page, with its bounding box in PostScript points.

    The origin of the bounding box is the top left corner of the page, and y
    coordinates increase downward, as in the output of "pdftotext -bbox-layout".
    """

    page: int  # 0-based page index
    xmin: float
    ymin: float
    xmax: float
    ymax: float
    text: str
    column: int = 0  # 0-based column index, set by assign_columns()
    # Whether this is a running head, a page number, or a line number in the margin.
    furniture: bool = False

    @property
    def height(self):
        """The height of the line, in PostScript points."""
        return self.ymax - self.ymin

    @property
    def word_count(self):
        """The number of words in the line."""
        return len(self.text.split())

    def normalized(self):
        """Return the line's text, lowercased and without a leading section number.

        A word that is set in small capitals is rejoined first, so that the initial
        letter of "R EFERENCES" is not mistaken for a section letter.

        Returns:
            The text of the line, in lowercase and without a section number.
        """
        text = SMALL_CAPS_WORD_RE.sub(r"\1\2", self.text.strip())
        return SECTION_NUMBER_RE.sub("", text).lower()

    def squeezed(self):
        """Return the line's text, lowercased and without spaces or punctuation.

        This canonicalizes headings such as "5. REFERENCES" and "R E F E R E N C E S".

        Returns:
            The text of the line, reduced to its lowercase letters.
        """
        return re.sub(r"[^a-z]", "", self.normalized())


@dataclass
class Page:
    """One page of the document."""

    width: float
    height: float
    lines: list = field(default_factory=list)

    def content_lines(self):
        """Return the lines that are text of the document rather than furniture.

        Returns:
            The lines of the page that are not marked as furniture.
        """
        return [line for line in self.lines if not line.furniture]


def die(message) -> NoReturn:
    """Write the message to standard error and exit with a failure status."""
    print("pdf-references.py: " + message, file=sys.stderr)
    sys.exit(2)


def run(command):
    """Run the command, returning its standard output.  Exit if the command fails.

    Returns:
        The standard output of the command.
    """
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    if result.returncode != 0:
        explanation = result.stderr.strip() or result.stdout.strip()
        die(f"command failed: {' '.join(command)}\n{explanation}")
    return result.stdout


def clean_text(text):
    """Return the text with ligatures expanded and stray whitespace removed.

    Returns:
        The text, with each ligature replaced by the characters it stands for and
        with each run of whitespace replaced by a single space.
    """
    for ligature, replacement in LIGATURES.items():
        text = text.replace(ligature, replacement)
    return " ".join(text.split())


def float_attribute(element, name, pdf_file):
    """Return the element's named attribute, as a float.  Exit if it is not a number.

    Returns:
        The value of the named attribute of the element.
    """
    value = element.get(name)
    tag = element.tag.removeprefix(XHTML)
    if value is None:
        die(f"pdftotext output for {pdf_file} has a <{tag}> with no {name} attribute")
    try:
        return float(value)
    except ValueError:
        die(
            f"pdftotext output for {pdf_file} has a <{tag}> whose {name} attribute"
            f" is not a number: {value!r}"
        )


def read_pages(pdf_file):
    """Return the pages of the PDF file, as a list of Page.

    Returns:
        The pages of the PDF file, with their furniture and columns marked.
    """
    if shutil.which("pdftotext") is None:
        die("cannot find program pdftotext, which is part of poppler")
    xml_text = INVALID_XML_CHARACTER_RE.sub(
        "", run(["pdftotext", "-bbox-layout", "-q", pdf_file, "-"])
    )
    try:
        root = ET.fromstring(xml_text)
    except ET.ParseError as e:
        die(f"cannot parse the output of pdftotext for {pdf_file}: {e}")
    pages = []
    for page_index, page_element in enumerate(root.iter(XHTML + "page")):
        page = Page(
            width=float_attribute(page_element, "width", pdf_file),
            height=float_attribute(page_element, "height", pdf_file),
        )
        for line_element in page_element.iter(XHTML + "line"):
            words = [
                clean_text(word_element.text or "")
                for word_element in line_element.iter(XHTML + "word")
            ]
            text = " ".join(word for word in words if word != "")
            if text == "":
                continue
            page.lines.append(
                Line(
                    page=page_index,
                    xmin=float_attribute(line_element, "xMin", pdf_file),
                    ymin=float_attribute(line_element, "yMin", pdf_file),
                    xmax=float_attribute(line_element, "xMax", pdf_file),
                    ymax=float_attribute(line_element, "yMax", pdf_file),
                    text=text,
                )
            )
        pages.append(page)
    if pages == []:
        die(f"{pdf_file} contains no pages")
    if all(page.lines == [] for page in pages):
        die(f"{pdf_file} contains no text; it may consist of scanned images")
    mark_furniture(pages)
    for page in pages:
        mark_line_numbers(page)
        assign_columns(page)
    return pages


def mark_furniture(pages):
    """Mark each line that is a running head, a page number, or a copyright notice.

    Such a line lies in the top or bottom margin and appears at the same height on
    more than one page.  Two page numbers count as the same text, because digits are
    ignored in the comparison.
    """
    candidates = collections.defaultdict(list)
    for page in pages:
        for line in page.lines:
            in_margin = (
                line.ymax <= MARGIN_FRACTION * page.height
                or line.ymin >= (1 - MARGIN_FRACTION) * page.height
            )
            if in_margin:
                key = (re.sub(r"[0-9]+", "#", line.text), round(line.ymin / 3))
                candidates[key].append(line)
    for lines in candidates.values():
        if len({line.page for line in lines}) >= 2:
            for line in lines:
                line.furniture = True


def overlapping_groups(lines):
    """Group the lines into maximal sets whose x coordinates overlap one another.

    Returns:
        A list of groups of lines, ordered by the leftmost x coordinate of each group.
    """
    groups = []
    for line in sorted(lines, key=lambda line: line.xmin):
        if groups != [] and line.xmin <= max(other.xmax for other in groups[-1]):
            groups[-1].append(line)
        else:
            groups.append([line])
    return groups


def mark_line_numbers(page):
    """Mark each line of the page that is a line number in the margin.

    A paper that is under submission is often typeset with a number beside every line
    of text, or beside every fifth line.  Such a number is recognized by standing in a
    vertical row of several numbers that increase downward and that lie beside the
    text rather than within it.

    The numbers of a numbered code listing satisfy this description too, and they are
    marked as well.  That is harmless, because a code listing is not a reference.
    """
    numbers = [line for line in page.lines if DIGITS_ONLY_RE.match(line.text)]
    others = [line for line in page.lines if not DIGITS_ONLY_RE.match(line.text)]
    for group in overlapping_groups(numbers):
        if len(group) < MIN_LINE_NUMBERS:
            continue
        group.sort(key=lambda line: line.ymin)
        values = [int(line.text) for line in group]
        if values != sorted(set(values)):
            continue
        xmin = min(line.xmin for line in group)
        xmax = max(line.xmax for line in group)
        ymin = min(line.ymin for line in group)
        ymax = max(line.ymax for line in group)
        # Text that shares both rows and columns with the numbers would mean that the
        # numbers stand within the text:  a column of a table, say.
        within_text = any(
            line.xmin < xmax and xmin < line.xmax and line.ymin < ymax and ymin < line.ymax
            for line in others
        )
        if not within_text:
            for line in group:
                line.furniture = True


def assign_columns(page):
    """Detect whether the page has two columns, and set the column of each line.

    A line that spans the two columns is assigned to the left column, so that the
    reading order (column, then y coordinate) treats it as part of the left column.
    Only one- and two-column layouts are recognized.
    """
    lines = page.content_lines()
    middle = page.width / 2
    left = [line for line in lines if line.xmax < middle]
    right = [line for line in lines if line.xmin > middle]
    spanning = [line for line in lines if line.xmin <= middle <= line.xmax]
    if len(left) >= 4 and len(right) >= 4 and len(spanning) <= max(2, 0.15 * len(lines)):
        # The x coordinate that separates the two columns.
        boundary = (max(line.xmax for line in left) + min(line.xmin for line in right)) / 2
        for line in page.lines:
            line.column = 1 if line.xmin > boundary else 0


def reading_order(pages):
    """Return the content lines of all pages, in reading order.

    The reading order is by page, then by column, then down the column.

    Returns:
        The content lines of all the pages, in reading order.
    """
    result = []
    for page in pages:
        result.extend(
            sorted(
                page.content_lines(),
                key=lambda line: (line.column, line.ymin, line.xmin),
            )
        )
    return result


def find_references_heading(lines, use_first, start_page):
    """Return the index in `lines` of the heading of the reference list.

    If `use_first`, return the first such heading rather than the last one.  If
    `start_page` is not None, consider only headings on that 0-based page.

    Returns:
        The index of the heading, or None if no heading was found.
    """
    candidates = [
        index
        for index, line in enumerate(lines)
        if line.squeezed() in REFERENCES_TITLES
        and line.word_count <= 12
        and (start_page is None or line.page == start_page)
    ]
    if candidates == []:
        return None
    return candidates[0] if use_first else candidates[-1]


def find_last_reference_line(lines, heading_index, margins):
    """Return the index in `lines` of the last line of the reference list.

    The reference list is assumed to end just before the heading that follows it.  A
    heading is recognized by its text, or by being taller (that is, set in a larger
    font) than a line of the reference list.  `margins` maps a page and column to its
    left and right margin, as computed by column_margins().

    Returns:
        The index of the last line of the reference list.
    """
    body = lines[heading_index + 1 :]
    if body == []:
        return heading_index
    # The lines just after the heading are part of the reference list, so their median
    # height is the height of a line of the reference list.  Taking the median rather
    # than the maximum tolerates a few taller lines, which occur if the reference list
    # is so short that these lines are not all part of it.
    reference_height = statistics.median(line.height for line in body[:15])
    for offset, line in enumerate(body):
        # A heading starts at the left margin, whereas an indented line is a
        # continuation of a reference.  A continuation line that contains a URL can be
        # as tall as a heading, because a URL is set in a different font.
        at_margin = line.xmin <= margins[line.page, line.column][0] + HEADING_MARGIN_TOLERANCE
        is_heading = (
            at_margin
            and not BRACKETED_LABEL_RE.match(line.text)
            and not NUMERIC_LABEL_RE.match(line.text)
            and (
                (line.word_count <= 12 and END_HEADING_RE.match(line.normalized()))
                or (line.word_count <= 8 and line.height >= 1.2 * reference_height)
            )
        )
        if is_heading:
            return heading_index + offset  # the line before this heading
    return len(lines) - 1


def label_starts(lines):
    """Return the indices in `lines` of lines that start a reference, by their labels.

    A bare number is accepted as a label only if the numbers of all the references
    are 1, 2, 3, ..., because a line begins with a number for other reasons too.

    Returns:
        The indices of the labeled lines, or an empty set if the references are
        not labeled.
    """
    bracketed = {index for index, line in enumerate(lines) if BRACKETED_LABEL_RE.match(line.text)}
    if len(bracketed) >= 2:
        return bracketed
    numbered = []
    for index, line in enumerate(lines):
        match = NUMERIC_LABEL_RE.match(line.text)
        if match is not None:
            numbered.append((index, int(match.group(1))))
    numbers = [number for (_, number) in numbered]
    if len(numbered) >= 3 and numbers == list(range(1, len(numbered) + 1)):
        return {index for (index, _) in numbered}
    return set()


def group_entries(lines):
    """Group the lines into references.

    Returns:
        The references, each one a list of Line, and a description of how the
        beginning of each reference was recognized.
    """
    starts = label_starts(lines)
    method = "labels such as [12]"
    if len(starts) < 2:
        starts = indentation_starts(lines)
        method = "indentation"
    if len(starts) < 2:
        starts = short_line_starts(lines)
        method = "the lengths of the lines"
    entries = []
    for index, line in enumerate(lines):
        if index in starts or entries == []:
            entries.append([])
        entries[-1].append(line)
    return entries, method


def column_margins(lines):
    """Return a map from (page, column) to the leftmost and rightmost x coordinate.

    Returns:
        A map from each (page, column) that appears in `lines` to a pair of its
        leftmost and rightmost x coordinate.
    """
    margins = {}
    for line in lines:
        key = (line.page, line.column)
        left, right = margins.get(key, (line.xmin, line.xmax))
        margins[key] = (min(left, line.xmin), max(right, line.xmax))
    return margins


def indentation_starts(lines):
    """Return the indices in `lines` of lines that start a reference, by indentation.

    In a hanging-indent layout, a reference starts at the left margin and its
    continuation lines are indented.  In a first-line-indent layout, the opposite
    holds.  The layout with fewer reference starts is the correct interpretation,
    because a reference occupies at least one line.

    Returns:
        The indices of the lines that start a reference, or an empty set if every
        line has the same indentation.
    """
    margins = column_margins(lines)
    flush = set()
    indented = set()
    for index, line in enumerate(lines):
        left = margins[line.page, line.column][0]
        if line.xmin <= left + INDENT_TOLERANCE:
            flush.add(index)
        else:
            indented.add(index)
    if indented == set() or flush == set():
        return set()
    return flush if len(flush) <= len(indented) else indented


def short_line_starts(lines):
    """Return the indices in `lines` of lines that start a reference, by line length.

    When the references are neither labeled nor indented, they are separated by
    vertical space, and the last line of a reference is shorter than a full line.

    Returns:
        The indices of the lines that start a reference.
    """
    margins = column_margins(lines)
    gaps = [
        second.ymin - first.ymax
        for first, second in itertools.pairwise(lines)
        if first.page == second.page and first.column == second.column
    ]
    typical_gap = statistics.median(gaps) if gaps != [] else 0.0
    starts = set()
    for index, line in enumerate(lines[1:], start=1):
        previous = lines[index - 1]
        left, right = margins[previous.page, previous.column]
        same_column = previous.page == line.page and previous.column == line.column
        if previous.xmax < right - 0.08 * (right - left) or (
            same_column and line.ymin - previous.ymax > typical_gap + 0.5 * line.height
        ):
            starts.add(index)
    return starts


def is_broken_url(text):
    """Return true if the text ends with a URL that continues on the next line.

    Returns:
        True if the last word of the text is a URL that ends in a character at
        which a URL is customarily broken.
    """
    last_word = text.rsplit(None, 1)[-1] if text.split() != [] else ""
    return ("://" in last_word or last_word.startswith("www.")) and re.search(
        r"[/.:~=&?_-]$", last_word
    ) is not None


def join_lines(lines, dehyphenate):
    """Return the text of the lines, joined into one line.

    If `dehyphenate`, a hyphen at the end of a line is removed when the next line
    begins with a lowercase letter.  That is usually right, but it is wrong for a
    word such as "object-oriented" that was broken at its own hyphen.

    Returns:
        The text of the lines, joined into one line.
    """
    result = ""
    for line in lines:
        text = line.text
        if result == "":
            result = text
        elif text.startswith("//") or is_broken_url(result):
            # A hyphen in a URL is part of the URL, not a sign of hyphenation.
            result = result + text
        elif dehyphenate and re.search(r"[a-zA-Z]-$", result) and re.match(r"[a-z]", text):
            result = result[:-1] + text
        else:
            result = result + " " + text
    return result


def formatted(text, width):
    """Return the text of one reference, wrapped to the given width.

    A width of 0 means not to wrap:  the reference occupies a single line.

    Returns:
        The text, with each line after the first indented by 4 spaces.
    """
    if width <= 0:
        return text
    return "\n".join(
        textwrap.wrap(text, width=width, subsequent_indent="    ", break_on_hyphens=False)
    )


def main():
    """Write the reference list of the PDF file that is named on the command line."""
    parser = argparse.ArgumentParser(
        description="Write a text file containing only a paper's bibliography, "
        "one reference per line.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""A PDF file records no boundary between one reference and the next,
so the output is a best effort; check it before relying on it.  If a heading is not
found, or the wrong one is, use --first, --start-page, --end-page, or --to-end.""",
    )
    parser.add_argument("pdf_file", help="the paper, in PDF format")
    parser.add_argument(
        "-o",
        "--output",
        help='the output file, or "-" for standard output '
        '(default: the input file with its extension replaced by "-refs.txt")',
    )
    parser.add_argument(
        "-w",
        "--width",
        type=int,
        default=0,
        help="wrap each reference to this many columns (default: do not wrap)",
    )
    parser.add_argument(
        "--blank-lines",
        action="store_true",
        help="write a blank line between references",
    )
    parser.add_argument(
        "--strip-labels",
        action="store_true",
        help='remove a label such as "[12]" from the beginning of each reference',
    )
    parser.add_argument(
        "--keep-hyphens",
        action="store_true",
        help="do not rejoin a word that is hyphenated across a line break",
    )
    parser.add_argument(
        "--raw",
        action="store_true",
        help="write each line of the PDF file as its own line, "
        "rather than joining the lines of a reference",
    )
    parser.add_argument(
        "--first",
        action="store_true",
        help='use the first "References" heading rather than the last one',
    )
    parser.add_argument(
        "--start-page",
        type=int,
        help='the 1-based page on which the "References" heading appears',
    )
    parser.add_argument(
        "--end-page",
        type=int,
        help="the 1-based page on which the reference list ends "
        "(default: determined automatically)",
    )
    parser.add_argument(
        "--to-end",
        action="store_true",
        help="the reference list extends to the end of the document",
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="print what was detected")
    args = parser.parse_args()

    pdf_path = pathlib.Path(args.pdf_file)
    if not pdf_path.is_file():
        die(f"no such file: {args.pdf_file}")
    output_file = args.output
    if output_file is None:
        output_file = str(pdf_path.with_name(pdf_path.stem + "-refs.txt"))
    if output_file != "-" and pathlib.Path(output_file).resolve() == pdf_path.resolve():
        die(f"the output file is the same as the input file: {output_file}")

    pages = read_pages(args.pdf_file)
    lines = reading_order(pages)

    start_page = None if args.start_page is None else args.start_page - 1
    if start_page is not None and not 0 <= start_page < len(pages):
        die(f"--start-page {args.start_page} is not in 1..{len(pages)}")
    heading_index = find_references_heading(lines, args.first, start_page)
    if heading_index is None:
        die(
            f'found no "References" or "Bibliography" heading in {args.pdf_file}'
            + ("" if start_page is None else f" on page {args.start_page}")
        )
    heading = lines[heading_index]

    if args.to_end:
        last_index = len(lines) - 1
    elif args.end_page is not None:
        if not 1 <= args.end_page <= len(pages):
            die(f"--end-page {args.end_page} is not in 1..{len(pages)}")
        on_end_page = [
            index
            for index, line in enumerate(lines)
            if line.page == args.end_page - 1 and index > heading_index
        ]
        if on_end_page == []:
            die(f"page {args.end_page} contains no text after the heading")
        last_index = on_end_page[-1]
    else:
        last_index = find_last_reference_line(lines, heading_index, column_margins(lines))
    last_line = lines[last_index]
    if last_line.page < heading.page:
        die(
            f"the reference list appears to end on page {last_line.page + 1}, before its "
            f"heading on page {heading.page + 1}; use --start-page, --end-page, or --to-end"
        )

    reference_lines = lines[heading_index + 1 : last_index + 1]
    if reference_lines == []:
        die(f'nothing follows the "{heading.text}" heading on page {heading.page + 1}')
    # Messages go to standard error if the references go to standard output.
    message_stream = sys.stderr if output_file == "-" else sys.stdout
    if args.raw:
        texts = [line.text for line in reference_lines]
    else:
        (entries, method) = group_entries(reference_lines)
        texts = [join_lines(entry, not args.keep_hyphens) for entry in entries]
        if args.verbose:
            print(
                f"Found the beginning of each reference by {method}.",
                file=message_stream,
            )
    if args.strip_labels:
        texts = [LABEL_RE.sub("", text).strip() for text in texts]

    separator = "\n\n" if args.blank_lines or args.width > 0 else "\n"
    text = separator.join(formatted(one, args.width) for one in texts) + "\n"
    if output_file == "-":
        sys.stdout.write(text)
    else:
        pathlib.Path(output_file).write_text(text, encoding="utf-8")
    if args.verbose or output_file != "-":
        noun = "line" if args.raw else "reference"
        if len(texts) != 1:
            noun = noun + "s"
        print(
            f"Wrote {len(texts)} {noun} to {output_file}, "
            f'from the "{heading.text}" heading on page {heading.page + 1} '
            f"through page {last_line.page + 1} of {args.pdf_file}.",
            file=message_stream,
        )


if __name__ == "__main__":
    main()
