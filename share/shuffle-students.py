#!/usr/bin/env python3
"""Print the students in a class-list CSV file, one per line, in random order.

Usage:  shuffle-students.py students.csv

The input is a CSV file, such as a UW class list, with a header row that
includes "FirstName" and "LastName" columns.  Each output line is
"FirstName LastName".  If no file is given, the CSV is read from standard input.
"""

import csv
import random
import sys


def main() -> None:
    if len(sys.argv) > 2:
        sys.exit(f"Usage: {sys.argv[0]} [students.csv]")
    if len(sys.argv) == 2:
        # "utf-8-sig" discards a byte-order mark, which would otherwise be
        # prepended to the first column name.
        with open(sys.argv[1], newline="", encoding="utf-8-sig") as f:
            rows = list(csv.DictReader(f))
    else:
        rows = list(csv.DictReader(sys.stdin))

    names = [f"{row['FirstName'].strip()} {row['LastName'].strip()}" for row in rows]
    random.shuffle(names)
    for name in names:
        print(name)


if __name__ == "__main__":
    main()
