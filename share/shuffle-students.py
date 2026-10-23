#!/usr/bin/env python3
"""List the students in a class-list CSV file, one per line, in random order.

Usage:  shuffle-students.py students.csv

The input is a CSV file, such as a UW class list, with a header row that
includes "FirstName" and "LastName" columns.  Each output line is
"FirstName LastName".  The list is printed to standard output and also written,
in a large font, to a PDF file with the same basename as the input (for
example, students.pdf).  Creating the PDF requires enscript and ps2pdf.
"""

import csv
import random
import subprocess
import sys
from pathlib import Path


def main() -> None:
    if len(sys.argv) != 2:
        sys.exit(f"Usage: {sys.argv[0]} students.csv")
    csv_path = Path(sys.argv[1])
    # "utf-8-sig" discards a byte-order mark, which would otherwise be
    # prepended to the first column name.
    with open(csv_path, newline="", encoding="utf-8-sig") as f:
        rows = list(csv.DictReader(f))

    names = [f"{row['FirstName'].strip()} {row['LastName'].strip()}" for row in rows]
    random.shuffle(names)
    text = "".join(f"{name}\n" for name in names)
    print(text, end="")
    write_pdf(text, csv_path.with_suffix(".pdf"))


def write_pdf(text: str, pdf_path: Path) -> None:
    """Write the text to a PDF file, in 24-point Helvetica with no page headers."""
    postscript = subprocess.run(
        ["enscript", "-q", "-B", "-f", "Helvetica24", "-M", "Letter",
         "--margins=54:54:54:54", "-o", "-"],
        input=text.encode("latin-1"), capture_output=True, check=True,
    ).stdout
    subprocess.run(["ps2pdf", "-", str(pdf_path)], input=postscript, check=True)


if __name__ == "__main__":
    main()
