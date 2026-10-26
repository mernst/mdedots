# Transcribing the instructor's handwritten comments

Notes for transcribing red-pen comments on scanned documents
and turning them into written feedback.

## Workflow

1. Skim the PDF a few pages at a time to find where the annotations are.
2. Render at 200 dpi (1700x2200 px per page) or higher:
   `pdftoppm -r 200 -png file.pdf p`
3. Crop each page into top and bottom halves and read them:
   `magick p-NN.png -crop 1700x1150+0+0 +repage t-NN.png` (top);
   use `+0+1050` for the bottom half.
   For hard words, crop tighter and add `-resize 200%`.
4. Marking uncertain readings with [?].
   Ask the instructor about each [?], giving its approximate location on the page.
5. To write feedback, expand each comment into a sentence addressed to the
   authors, grouped by section. Don't add critiques that aren't in the comments.
   List every interpretation of a terse comment in a separate review-notes file.

## Handwriting quirks (all caps, red ink)

- "I" has serifs and can look like "Z" or "T". So "IS" can look like "ZS" or "I5",
  and "IT" like "ZT".
- "E" often looks like "Σ" or "L". "A" often lacks its crossbar.
- "N" can look like "IV", "W" like "VV", and "?" like "7".
- "Def?" has a slanted D and a crossed f, so it can look like "Near?", "Next?" or
  "Dex?". A short word plus "?" next to a jargon term is probably "Def?".
- "Dispreferences" (things a user does *not* want) is a real word here. Don't
  "correct" it to "display preferences".

## Recurring comments and marks

- **✓ = "Well done."** It is praise, not just "seen" or "OK". Write it as
  "Well done." in feedback, and count it as positive feedback.
- **"Def?"** = please define this term.
- Other common comments: "Evidence?", "How?", "Why?", "Vague", "Generic", "Weak",
  "Say earlier", "Spell out", "Not in abstract", "Out of scope", "buy-in", "Is it?".
- A face (smiling, neutral or frowning) at the top of a pitch's first page is the
  overall verdict.
- A strikethrough or X through a word means delete it.
