* Several features require `ghostscript`.  To make N-up pdf documents you'll also need `pdfxup`.
  The examples and tests shouldn't throw an ERROR if one (or all)
  of these are not installed.
* This is a resubmission which makes the following tweaks requested by CRAN:

  + Removes "Functions for" at the start of the description.
  + Added `\value` tag to the Rd files that were missing it.
  + Replaced all the `\dontrun{}` examples with runnable examples.
    The examples that previously needed a PDF file the user was not expected to have
    now generate a mock PDF file in `tempdir()` which is then cleaned up.
  + No longer should accidentally write to the user's home filespace.
    `pdf_width()`/`pdf_height()` no longer create an `Rplots.pdf` file in the working directory
    and the examples and tests only write to `tempdir()`.
  + Removed `:::` from two of the four flagged examples.
    The remaining two examples use `:::` only to call this package's **own** internal helpers
    (never another package's).  The API stability concern in "Writing R Extensions" is about
    accessing *other* packages' unexported objects.  Since `R CMD check` runs these examples,
    any change to these helpers would be caught by our own checks immediately.

## Test environments

* local (linux, R 4.6.1)
* win-builder (windows, R devel)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

1 NOTE (that this is a new submission)
