// hotcrp-fill-submission.js
//
// Fills in (and optionally saves) a HotCRP "New submission" form
// (https://<site>.hotcrp.com/paper/new) from a data object.
// Written 2026-09-27 to enter the 590n papers (590n-papers-urls.txt) into
// https://uw-serg26.hotcrp.com.  Run it in the page with Claude-in-Chrome's
// javascript_tool (it uses top-level await, so it must run as an async body).
//
// ---------------------------------------------------------------------------
// HOW TO USE (workflow that worked)
// ---------------------------------------------------------------------------
// 1. Gather metadata per paper: title, authors [[name, affiliation], ...],
//    abstract, PDF URL.  For papers with a PDF: `curl` it, `pdftotext -l 2`,
//    extract the abstract between known start/end phrases, join lines, then
//    fix words that pdftotext merged at line-end hyphens (e.g.,
//    "semanticspreserving" -> "semantics-preserving"): list words longer than
//    9 characters that are not in /usr/share/dict/words and inspect them.
//    For papers without a PDF, the publisher tables of contents at
//    https://www.conference-publishing.com/toc/ISSTA26 (or ASE26, etc.)
//    list authors, affiliations, and abstracts (the ACM DL is behind Cloudflare).
//
// 2. To avoid pasting large scripts repeatedly, store the data and this code
//    in the site's localStorage once (same origin, so it persists across
//    navigations):
//      localStorage.setItem('serg_papers', JSON.stringify({
//        "3": [title, abstract, [[name, affil], ...], pdfUrlOrNull], ...}));
//      localStorage.setItem('serg_code', <the body of this file below the
//        "BEGIN BODY" line, as a string>);
//
// 3. For each paper: navigate the tab to /paper/new, then run:
//      const NUM = "3"; const P = JSON.parse(localStorage.serg_papers)[NUM];
//      const D = {num: NUM, title: P[0], abstract: P[1], authors: P[2], pdf: P[3]};
//      if (location.pathname !== '/paper/new') "WRONG PAGE";
//      else await (new (Object.getPrototypeOf(async function(){}).constructor)
//                   ('D', localStorage.serg_code))(D)
//    Batch navigate + run + wait(6s) + check `location.pathname` with
//    browser_batch; on success the tab ends at /paper/<id>/edit.
//
// ---------------------------------------------------------------------------
// FIELDS OF D
// ---------------------------------------------------------------------------
//   D.num      paper number (used only for the uploaded file name).
//   D.title, D.abstract   strings.
//   D.authors  [[name, affiliation], ...].  Every author's email is set to
//              EMAIL below; the user did not want real author emails used.
//              HotCRP adds a new blank author row automatically when the last
//              one is filled, so any number of authors works.
//   D.pdf      URL of the PDF, fetched from within the page.  That requires
//              the PDF host to send Access-Control-Allow-Origin (arXiv,
//              github.io, raw.githubusercontent.com do; check with
//              `curl -sI URL | grep -i access-control`).  If it does not:
//                - set D.pdf = null and D.nosave = true, run this script,
//                - create the hidden file input (see the click override below,
//                  or run just those 3 lines), locate it with `find`
//                  ("file input for submission PDF"), and upload the local file
//                  with the file_upload tool (a scratchpad path works),
//                - then check status:submit, uncheck status:notify, and click
//                  button[name=update].
//              Do NOT fetch from a localhost server: Chrome's local-network
//              permission prompt hangs the page.
//              With no PDF at all, HotCRP disables "ready for review", and the
//              only option is "Save draft" (status "No submission").
//   D.nosave   if true, fill the form but do not click save.
//
// Adding a PDF to an EXISTING paper (e.g., a draft) after the deadline:
// open /paper/<id>/edit?forceShow=1 (without forceShow the admin view is
// read-only), create the file input as described above, use file_upload,
// check status:submit, uncheck status:notify, then click
// button.js-override-deadlines (there is no button[name=update]).  That opens
// HotCRP's in-page "override the deadline?" dialog (not a browser alert); click
// its "Save and submit" button.
//
// Behavior: unchecks "Email authors" (status:notify), checks "The submission is
// ready for review" (status:submit), and, unless D.nosave, clicks the
// "Save and submit" button (button[name=update]) 800ms after returning, but only
// if the sanity checks pass; otherwise the result has NOT_SAVED: true.
// The result deliberately omits email values, because the tool redacts results
// that contain them.
//
// ---------------------------------------------------------------------------
// BEGIN BODY
// ---------------------------------------------------------------------------
const EMAIL = "mernst+serg@cs.washington.edu";
function set(el, v) {
  el.focus(); el.value = v;
  el.dispatchEvent(new Event('input', {bubbles: true}));
  el.dispatchEvent(new Event('change', {bubbles: true}));
  el.blur();
}
set(document.querySelector('[name=title]'), D.title);
set(document.querySelector('[name=abstract]'), D.abstract);
for (let i = 0; i < D.authors.length; i++) {
  let emails = document.querySelectorAll('input[placeholder=Email]');
  if (i >= emails.length) {  // wait for HotCRP to append another author row
    await new Promise(r => setTimeout(r, 300));
    emails = document.querySelectorAll('input[placeholder=Email]');
  }
  set(emails[i], EMAIL);
  set(document.querySelectorAll('input[placeholder=Name]')[i], D.authors[i][0]);
  set(document.querySelectorAll('input[placeholder=Affiliation]')[i], D.authors[i][1]);
}
let up = "no pdf";
if (D.pdf) {
  const blob = await (await fetch(D.pdf)).blob();
  // HotCRP's "Upload" button (#submission:uploader) creates a hidden
  // <input type=file> and calls .click() on it, which would open a native file
  // picker that automation cannot operate.  Suppress that click, then assign
  // the file via DataTransfer and fire "change" so HotCRP picks it up.
  const origClick = HTMLInputElement.prototype.click;
  HTMLInputElement.prototype.click = function () { if (this.type !== 'file') return origClick.call(this); };
  document.getElementById('submission:uploader').click();
  HTMLInputElement.prototype.click = origClick;
  const inp = document.querySelector('input[type=file]');
  const dt = new DataTransfer();
  dt.items.add(new File([blob], "paper" + D.num + ".pdf", {type: "application/pdf"}));
  inp.files = dt.files;
  inp.dispatchEvent(new Event('change', {bubbles: true}));
  up = blob.size;
}
// Use click() rather than setting .checked, so HotCRP's handlers run.
const notify = document.querySelector('input[name="status:notify"]'); if (notify && notify.checked) notify.click();
const sub = document.querySelector('input[name="status:submit"]'); if (sub && !sub.checked) sub.click();
await new Promise(r => setTimeout(r, 300));
const E = document.querySelectorAll('input[placeholder=Email]'),
      N = document.querySelectorAll('input[placeholder=Name]'),
      A = document.querySelectorAll('input[placeholder=Affiliation]');
const R = {
  up, notify: notify && notify.checked, submit: sub && sub.checked,
  buttons: [...document.querySelectorAll('button.js-savepaper')].map(b => b.textContent),
  rows: [...N].map((n, i) => n.value + " / " + A[i].value).filter(s => s !== " / "),
  okEmails: [...E].filter(e => e.value === EMAIL).length,
  title: document.querySelector('[name=title]').value,
};
if (!D.nosave && R.submit && !R.notify && R.okEmails === D.authors.length && R.title === D.title)
  setTimeout(() => document.querySelector('button[name=update]').click(), 800);
else R.NOT_SAVED = true;
return R;
