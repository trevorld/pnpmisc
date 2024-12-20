#' Layout data frame for a grid of identically sized components
#'
#' `layout_grid()` calculates a layout data frame
#' for a grid of identically sized print-and-play components.
#' @param nrow,ncol Number of rows and columns of print-and-play components (e.g. cards)
#' @param width,height,bleed Width, height, and bleed for each print-and-play component (in inches)
#' @param paper,orientation print-and-play paper size and orientation
#' @examples
#' # Button Shy card layout
#' layout_grid(nrow = 2L, ncol = 3L, height = 3.447, width = 2.469, bleed = 0.125)
#' @return A data frame with columns "row", "col", "x", "y", "width", "height", "bleed", "paper", and "orientation".
#' @export
layout_grid <- function(nrow = 2L, ncol = 1L,
                        width = 2.5, height = 3.5, bleed = 0,
                        paper = c("letter", "a4"), orientation = c("landscape", "portrait")) {
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
    paper <- tolower(paper)
    paper <- match.arg(paper)
    orientation <- match.arg(orientation)

    stopifnot(nrow > 0L, ncol > 0L)

    xc <- 0.5 * paper_width(paper, orientation)
    yc <- 0.5 * paper_height(paper, orientation)

    if (ncol == 1L) {
        x <- xc
    } else if (is_odd(ncol)) {
        m <- ceiling(ncol / 2)
        xl <- rev(seq(from = xc, by = -(width + 2 * bleed), length.out = m)[-1L])
        xr <- seq(from = xc, by = (width + 2 * bleed), length.out = m)[-1L]
        x <- c(xl, xc, xr)
    } else {
        m <- ncol / 2
        xl <- rev(seq(from = xc - 0.5 * (width + 2 * bleed), by = -(width + 2 * bleed), length.out = m))
        xr <- seq(from = xc + 0.5 * (width + 2 * bleed), by = (width + 2 * bleed), length.out = m)
        x <- c(xl, xr)
    }
    if (nrow == 1L) {
        y <- yc
    } else if (is_odd(nrow)) {
        m <- ceiling(nrow / 2)
        yl <- rev(seq(from = yc, by = -(height + 2 * bleed), length.out = m)[-1L])
        yr <- seq(from = yc, by = (height + 2 * bleed), length.out = m)[-1L]
        y <- c(yl, yc, yr)
    } else {
        m <- nrow / 2
        yl <- rev(seq(from = yc - 0.5 * (height + 2 * bleed), by = -(height + 2 * bleed), length.out = m))
        yr <- seq(from = yc + 0.5 * (height + 2 * bleed), by = (height + 2 * bleed), length.out = m)
        y <- c(yl, yr)
    }
    data.frame(row = rep(seq.int(nrow), each = ncol),
               col = rep(seq.int(ncol), nrow),
               x = rep(x, nrow),
               y = rep(rev(y), each = ncol),
               width = width, height = height, bleed = bleed,
               paper = paper, orientation = orientation)
}

#' Layout data frame for a named preset
#'
#' `layout_preset()` calculates a layout data frame
#' for a named preset.
#' `layout_names()` returns the supported layout presets.
#'
#' Unless otherwise indicated assumes letter-sized paper.  Supports the following presets:
#'
#' \describe{
#' \item{button_shy_cards}{[Button Shy Games](https://buttonshygames.com/collections/pnps) PnP cards.
#'                         **Caveat:** matches many but not **all** of their PnP files e.g. `ROVE-PNP-V2.pdf` better matches the `poker_3x2_bleed` layout.}
#' \item{button_shy_rules}{[Button Shy Games](https://buttonshygames.com/collections/pnps) PnP rule booklet pages.}
#' \item{poker_3x2_bleed}{Poker-sized cards (2.5" by 3.5") in 3 columns of 2 cards (landscape) with an 1/8" bleed around each card.  Examples of PnP games using this layout include [Galdor's Grip](https://greggjewell.itch.io/galdors-grip).}
#' \item{poker_3x3}{Poker-sized cards (2.5" by 3.5") in 3 columns of 3 cards (portrait) with an zero bleed around each card.  Examples of PnP games using this layout include the original [Mini Rogue](https://boardgamegeek.com/boardgame/199242/mini-rogue-a-roguelike-microgame).}
#' \item{poker_4x2}{Poker-sized cards (2.5" by 3.5") in 4 columns of 2 cards (landscape) with zero bleed around each card.  Examples of PnP games using this layout include the [Decktet](https://www.decktet.com/getit.php).}
#' }
#'
#' @param name Preset name.  Must be in `layout_names()`.
#' @examples
#' layout_preset("button_shy_cards")
#' layout_names()
#' @return A data frame with columns "row", "col", "x", "y", "width", "height", "bleed", "paper", and "orientation".
#' @export
layout_preset <- function(name = "button_shy_cards") {
    name <- match.arg(name, layout_names())
    switch(name,
           button_shy_cards = layout_grid(nrow = 2L, ncol = 3L, height = 3.447, width = 2.469, bleed = 0.125),
           button_shy_rules = layout_grid(nrow = 2L, ncol = 4L),
           poker_3x2_bleed = layout_grid(nrow = 2L, ncol = 3L, bleed = 0.125),
           poker_3x3 = layout_grid(nrow = 3L, ncol = 3L, orientation = "portrait"),
           poker_4x2 = layout_grid(nrow = 2L, ncol = 4L)
           )
}

#' @rdname layout_preset
#' @export
layout_names <- function() c("button_shy_cards", "button_shy_rules",
                             "poker_3x2_bleed", "poker_3x3", "poker_4x2")

# To check fit of presets try something like
# input <- "tmp/galdors_grip.pdf"
# vp = viewport(width = unit(11, "in"), height = unit(8.5, "in"))
# pm = pdf_render_bm_pixmap(input, page = 1)
# df = layout_grid(nrow = 2, ncol = 3, height = 3.500, width = 2.500, bleed = 0.125)
# # df = layout_preset("poker_3x2_bleed")
# grid.newpage(); pushViewport(vp); grid.raster(pm); grid_draw_lines(layout=df, gp=gpar(col="red", lty="dashed"))

# Birdscaping after padding to letter size and rotating 90 degrees is close to
# layout_grid(nrow = 2, ncol = 4, height = 3.405, width = 2.425, bleed = 0.125)

draw_hline <- function(y = unit(0.5, "npc"), ...) {
    grid.segments(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                  y0 = y, y1 = y, ...)
}
draw_vline <- function(x = unit(0.5, "npc"), ...) {
    grid.segments(y0 = unit(0, "npc"), y1 = unit(1, "npc"),
                  x0 = x, x1 = x, ...)
}

#' Draw lines along component edges
#'
#' `grid_add_lines()` draws lines along the components of a print-and-play layout.
#'
#' * This function draws in **inches** so make sure your graphics device is "big" enough.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_add_crosshairs
#' @param gp Passed to [grid::grid.segments()].
#' @param ... Ignored for now.
#' @return `NULL` invisibly.
#'         As a side effect draws rectangles to the active graphics device.
#' @seealso [grid::grid.segments()]
#' @examples
#' grid::grid.newpage()
#' vp <- grid::viewport(width=8.5, height=11, default.units="in",
#'                      x=0.0, y=0.0, just=c("left", "bottom"))
#' grid::pushViewport(vp)
#' grid_add_lines(layout = "poker_3x3",
#'                gp = grid::gpar(lty = "dashed", col = "grey"))
#' grid::popViewport()
#' @export
grid_add_lines <- function(..., layout = "poker_3x3", gp = gpar()) {
    if (is.character(layout))
        layout <- layout_preset(layout)

    for (i in seq_len(nrow(layout))) {
        dy <- 0.5 * (layout$height[i])
        dx <- 0.5 * (layout$width[i])
        draw_hline(unit(layout$y[i] + dy, "in"), gp = gp)
        draw_hline(unit(layout$y[i] - dy, "in"), gp = gp)
        draw_vline(unit(layout$x[i] + dx, "in"), gp = gp)
        draw_vline(unit(layout$x[i] - dx, "in"), gp = gp)
    }
    invisible(NULL)
}
