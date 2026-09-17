* Several features require `ghostscript`.  To make N-up pdf documents you'll also need `pdfxup`.
  The examples and tests shouldn't throw an ERROR if one (or all)
  of these are not installed.
* Makes the following tweaks requested by CRAN:

  + Removes "Functions for" at the start of the description.
  + Added `\value` tag to the Rd files that were missing it.

## Test environments

* local (linux, R 4.6.1)
* win-builder (windows, R devel)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

1 NOTE (that this is a new submission)
