# pnpmisc

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/pnpmisc)](https://cran.r-project.org/package=pnpmisc)
[![R-CMD-check](https://github.com/trevorld/pnpmisc/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/pnpmisc/actions)
[![Coverage Status](https://codecov.io/gh/trevorld/pnpmisc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/trevorld/pnpmisc)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Related links](#related)

## <a name="overview">Overview</a>

`{pnpmisc}` is an R package to edit pdf files.
While some of the pdf editing functions are of general use the motivating goal of this package
is to help me tweak existing print-and-play files to a format a little
bit easier for me to physically manufacture using my preferred techniques and tools.

The pdf editing functions in this package have the following conventions which allow them to be chained:

* The input filename is the first positional argument.
* The output filename is the second positional argument and if unspecified defaults to `tempfile(fileext=".pdf")`.
* All other arguments must be named.
* Returns the output filename invisibly.

My personal print-and-play preferences:

* I prefer crop marks and/or cross hairs instead of full cut lines.  Cross hairs should only be on one side of a duplex print.
* I prefer my pdf files to be letter sized.
* I prefer to print cards duplex.  To forgive a bit of front-back printing drift there should be enough bleed around the cards and any cross hairs should just be on one side.
* I prefer when folding to know where I should line up the (trimmed) paper edge.

## <a name="installation">Installation</a>


``` r
remotes::install_github("trevorld/pnpmisc")
```

## <a name="examples">Examples</a>

### `pdf_add_origami()`

* Sometimes when folding paper with a tool like a bone folder it is easier to fold if you
know where to precisely place a trimmed paper edge.
* `pdf_add_origami()` adds some origami symbols to
[Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home).


``` r
library("pnpmisc")
# Download Boardgame Barrio Small Box Game Jackets from
# <https://sites.google.com/view/boardgamebarrio/home>
input <- "SBG_Jacket_-_Animal_Upon_Animal.pdf"
output <- "animal_upon_animal_jacket.pdf"
input |> pdf_gs() |>
    pdf_subset(pages = 1L) |>
    pdf_add_origami() |>
    pdf_clean(output)
```

### `pdf_pad_pagesize()`

* Since "letter" sized paper is shorter but wider than "A4" sized paper some print-and-play files have a
weird shared page size that is a bit smaller than both "letter" and "A4" paper sizes.
* `pdf_pad_pagesize()` pads the size of such files to reach the full letter or A4 paper size without scaling the original component images and making sure the original images stay centered in the page (e.g. for "duplex" printing).


``` r
library("pnpmisc")
# Download Birdscaping from <https://www.pnparcade.com/products/birdscaping>
input <- "Birdscaping 1.8 - Front-Back Layout.pdf"
output <- "birdscaping_cards.pdf"
pdf_pad_pagesize(input, output)
```

### `pdf_rm_crosshairs()`

* Since there may be some font-back printing drift when duplex printing cards
  I prefer only crosshairs on one side of the double-sided leaf so the both sides of the cards look clean after removing the corners.
* `pdf_rm_crosshairs()` removes the crosshairs from [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (v1, letter).
* By default removes the crosshairs from the odd pages but can also remove them from the even pages.


``` r
library("pnpmisc")
# Download Galdor's Grip from <https://greggjewell.itch.io/galdors-grip>
input <- "GaldorsGrip_PnP_Cards_TGCBundle_EN_USL_v1.pdf"
output <- "galdors_grip_cards.pdf"
pdf_rm_crosshairs(input, output, pages = "odd")
```

## <a name="related">Related links</a>

### R packages

* [papersize](https://github.com/elipousson/papersize) has some functionality to create print-and-play playing card layouts.
* [pdftools](https://github.com/ropensci/pdftools), [qpdf](https://github.com/ropensci/qpdf), [staplr](https://github.com/pridiltal/staplr), and [xmpdf](https://github.com/trevorld/r-xmpdf) are some pdf manipulation packages.
* [piecepackr](https://github.com/piecepackr/piecepackr) has some graphical functionality for creating print-and-play layouts.

### Some print-and-play links

* [Boardgame Barrio](https://sites.google.com/view/boardgamebarrio)'s Small Box Game Jackets
* [Button Shy Games](https://buttonshygames.com/)
* [Decktet](https://www.decktet.com/) Card Game System
* [Galdor's Grip](https://greggjewell.itch.io/galdors-grip)
* [Martin's Print and Play Hideaway](https://www.facebook.com/groups/pnphideaway/) Facebook group
* [One Card Maze](https://onecardmaze.com/)
* [Piecepack](https://ludism.org/ppwiki/Downloadable_Piecepack_Sets) Board Game System
