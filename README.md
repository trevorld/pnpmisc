# pnpmisc

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/pnpmisc)](https://cran.r-project.org/package=pnpmisc)
[![R-CMD-check](https://github.com/trevorld/pnpmisc/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/pnpmisc/actions)
[![Coverage Status](https://codecov.io/gh/trevorld/pnpmisc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/trevorld/pnpmisc)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [`pdf_pad()`](#pdf-pad)

* [Related links](#related)

## <a name="overview">Overview</a>

`{pnpmisc}` is an R package to help me tweak existing print-and-play files to a format a little
bit easier for me to physically manufacture using my preferred techniques and tools:

* I prefer crop marks and/or cross hairs instead of full cut lines.  Cross hairs should only be on one side of a duplex print.
* I prefer my pdf files to be letter sized.
* I prefer to print cards duplex.  To forgive a bit of front-back printing drift there should be enough bleed around the cards and any cross hairs should just be on one side.
* I prefer when folding to know where I should line up the (trimmed) paper edge.

## <a name="installation">Installation</a>


``` r
remotes::install_github("trevorld/pnpmisc")
```

## <a name="examples">Examples</a>

### <a name="pdf-pad">`pdf_pad()`</a>

Since "letter" sized paper is shorter but wider than "A4" sized paper some print-and-play files have
weird shared page size that is a bit smaller than both "letter" and "A4" paper sizes. `pdf_pad()` pads the size of such files to reach the full letter or A4 paper size without scaling the original component images and making sure the original images stay centered in the page (e.g. for "duplex" printing).


``` r
# Download Birdscaping from <https://www.pnparcade.com/products/birdscaping>
input <- "Birdscaping 1.8 - Front-Back Layout.pdf"
output <- "birdscaping_letter.pdf"
pdf_pad(input, output)
```

## <a name="related">Related links</a>

### R packages

* [papersize](https://github.com/elipousson/papersize) has some functionality to print-and-play playing cards.
* [pdftools](https://github.com/ropensci/pdftools) imports pdf file pages as bitmap images.
* [piecepackr](https://github.com/piecepackr/piecepackr) has some graphical functionality for print-and-play files like crop marks.
