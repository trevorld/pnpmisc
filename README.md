# pnpmisc

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/pnpmisc)](https://cran.r-project.org/package=pnpmisc)
[![R-CMD-check](https://github.com/trevorld/pnpmisc/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/pnpmisc/actions)
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

You can install the development version using my [R-universe](https://ropensci.org/r-universe/) universe:


``` r
install.packages('pnpmisc', repos = c('https://trevorld.r-universe.dev', 'https://cloud.r-project.org'))
```

or by using the `{remotes}` package:


``` r
remotes::install_github("trevorld/pnpmisc")
```

## <a name="examples">Examples</a>

### `pdf_add_crosshairs()`

* The old rotary trimmer I use to cut cards doesn't let me cut only the interior of a laminated sheet of cards so it is nice if one side has crosshairs at the corner of each card to help guide cuts after the outside crop marks have been cut away.
* `pdf_add_crosshairs()` adds such crosshairs to the corners of the cards on the indicated pages.


``` r
library("piecepackr")
library("pnpmisc")
# Download "A Nice Cuppa"
# <https://www.pnparcade.com/collections/button-shy-games>
input <- "A Nice Cuppa - PNP.pdf"
output <- "a_nice_cuppa_cards.pdf"
input |> pdf_subset(pages = -1L) |>
    pdf_add_crosshairs(output, layout = "button_shy_cards", pages = "even")
rm_temp_pdfs()
```

### `pdf_add_rects()`

* Border lines don't look very clean when manually cutting cards out.
  Instead I'd prefer no border lines and instead some crosshairs on the card corners of one side
  (which will get cleanly removed after cutting the corners off).
* If the PnP layout has a white background you sometimes can "remove" unwanted border lines by
  covering them up with new white border lines.


``` r
library("piecepackr")
library("pnpmisc")
# Download Mini Rogue (Original)
# <https://boardgamegeek.com/boardgame/199242/mini-rogue-a-roguelike-microgame/files>
input <- "Mini_Rogue_-_Cards_v1.2.1.pdf"
output <- "mini_rogue_cards.pdf"
input |> pdf_subset(pages = 1:2) |>
    pdf_add_rects(layout = "poker_3x3", 
                  gp = grid::gpar(fill = NA, col = "white", lwd = 2)) |>
    pdf_add_crosshairs(output, pages = 2L, layout = "poker_3x3")
rm_temp_pdfs()
```

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
    pdf_add_origami(output)
rm_temp_pdfs()
```

### `pdf_create_wallet()`

* I like to store my smaller print-and-play card games (i.e. up to 36 cards or so) in an origami card wallet.
* `pdf_create_wallet()` can create a customizable print-and-play layout for an origami card wallet.


``` r
library("bittermelon")
library("grid")
library("gridpattern")
library("pnpmisc")
# Download "A Nice Cuppa"
# <https://www.pnparcade.com/collections/button-shy-games>
input <- "A Nice Cuppa - PNP.pdf"
output <- "a_nice_cuppa_wallet.pdf"
bm_cover <- pdf_render_bm_pixmap(input, page = 1L) |>
  bm_crop_layout(layout = "button_shy_rules", row = 1L, col = 4L)
bm_sip <- pdf_render_bm_pixmap(input, page = 4L) |>
  bm_crop_layout(layout = "button_shy_cards", row = 1L, col = 1L) |>
  bm_trim(bottom = 210, top = 290, left = 110, right = 80)
bm_back <- pdf_render_bm_pixmap(input, page = 5L) |>
  bm_crop_layout(layout = "button_shy_cards", row = 1L, col = 1L) |>
  bm_rotate(90)
basket <- patternGrob("weave", type = "basket",
                      fill = "#8A624A", fill2 = "#8A624A",
                      angle = 0, spacing = 0.1, density = 1.0)
front <- gList(basket, rasterGrob(bm_cover, width = unit(1.8, "in")))
back <- gList(basket, rasterGrob(bm_sip, width = unit(1.8, "in")))
spine <- rasterGrob(bm_back, width = unit(8.25, "in"))
pdf_create_wallet(output, front = front, back = back, spine = spine, bleed = 0.125)
```

### `pdf_pad_paper()`

* Since "letter" sized paper is shorter but wider than "A4" sized paper some print-and-play files have a
weird shared page size that is a bit smaller than both "letter" and "A4" paper sizes.
* `pdf_pad_paper()` pads the size of such files to reach the full letter or A4 paper size without scaling the original component images and making sure the original images stay centered in the page (e.g. for "duplex" printing).


``` r
library("pnpmisc")
# Download Birdscaping from <https://www.pnparcade.com/products/birdscaping>
input <- "Birdscaping 1.8 - Front-Back Layout.pdf"
output <- "birdscaping_cards.pdf"
pdf_pad_paper(input, output)
```

### `pdf_rm_crosshairs()`

* Since there may be some font-back printing drift when duplex printing cards
  I prefer only crosshairs on one side of the double-sided leaf so the both sides of the cards look clean after removing the corners.
* `pdf_rm_crosshairs()` removes the crosshairs from a PnP layout with a solid color bleed.
* By default removes the crosshairs from the odd pages but can also remove them from the even pages (or any subset supported by `pdf_pages()`).
* The default `layout` supports [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (v1, letter).


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
