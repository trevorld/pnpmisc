#' Layout data frame for a grid of identically sized components
#'
#' `layout_grid()` calculates a layout data frame
#' for a grid of identically sized print-and-play components.
#' @param nrow,ncol Number of rows and columns of print-and-play components (e.g. cards)
#' @param width,height,bleed Width, height, and bleed for each print-and-play component (in inches)
#' @param paper,orientation print-and-play paper size and orientation
#' @examples
#' # Button Shy card layout
#' layout_grid(nrow = 2L, ncol = 3L, height = 3.447, width = 2.469, bleed = 0.25)
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
        xl <- rev(seq(from = xc, by = -(width + bleed), length.out = m)[-1L])
        xr <- seq(from = xc, by = (width + bleed), length.out = m)[-1L]
        x <- c(xl, xc, xr)
    } else {
        m <- ncol / 2
        xl <- rev(seq(from = xc - 0.5 * (width + bleed), by = -(width + bleed), length.out = m))
        xr <- seq(from = xc + 0.5 * (width + bleed), by = (width + bleed), length.out = m)
        x <- c(xl, xr)
    }
    if (nrow == 1L) {
        y <- yc
    } else if (is_odd(nrow)) {
        m <- ceiling(nrow / 2)
        yl <- rev(seq(from = yc, by = -(height + bleed), length.out = m)[-1L])
        yr <- seq(from = yc, by = (height + bleed), length.out = m)[-1L]
        y <- c(yl, yc, yr)
    } else {
        m <- nrow / 2
        yl <- rev(seq(from = yc - 0.5 * (height + bleed), by = -(height + bleed), length.out = m))
        yr <- seq(from = yc + 0.5 * (height + bleed), by = (height + bleed), length.out = m)
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
#' @param name Preset name.  Must be in `layout_names()`.
#' @examples
#' layout_preset("button_shy_cards")
#' layout_names()
#' @return A data frame with columns "row", "col", "x", "y", "width", "height", "bleed", "paper", and "orientation".
#' @export
layout_preset <- function(name = "button_shy_cards") {
    name <- match.arg(name, layout_names())
    switch(name,
           button_shy_cards = layout_grid(nrow = 2L, ncol = 3L, height = 3.447, width = 2.469, bleed = 0.25)
           )
}

#' @rdname layout_preset
#' @export
layout_names <- function() c("button_shy_cards")

# To check fit of presets try something like
# vp = viewport(width = unit(11, "in"), height = unit(8.5, "in"))
# pm = pdf_render_bm_pixmap("tmp/A Nice Cuppa - PNP.pdf", page = 2)
# df = layout_grid(nrow = 2, ncol = 3, height = 3.447, width = 2.469, bleed = 0.25)
# grid.newpage(); pushViewport(vp); grid.raster(pm); draw_layout(df)

draw_hline <- function(y = unit(0.5, "npc"), ...) {
    grid.segments(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                  y0 = y, y1 = y, ...)
}
draw_vline <- function(x = unit(0.5, "npc"), ...) {
    grid.segments(y0 = unit(0, "npc"), y1 = unit(1, "npc"),
                  x0 = x, x1 = x, ...)
}
draw_layout <- function(df, solid = "white", dashed = "black") {
    for (i in seq_len(nrow(df))) {
        dy <- 0.5 * (df$height[i])
        dx <- 0.5 * (df$width[i])
        draw_hline(unit(df$y[i] + dy, "in"), gp = gpar(col = solid))
        draw_hline(unit(df$y[i] + dy, "in"), gp = gpar(col = dashed, lty = "dashed"))
        draw_hline(unit(df$y[i] - dy, "in"), gp = gpar(col = solid))
        draw_hline(unit(df$y[i] - dy, "in"), gp = gpar(col = dashed, lty = "dashed"))
        draw_vline(unit(df$x[i] + dx, "in"), gp = gpar(col = solid))
        draw_vline(unit(df$x[i] + dx, "in"), gp = gpar(col = dashed, lty = "dashed"))
        draw_vline(unit(df$x[i] - dx, "in"), gp = gpar(col = solid))
        draw_vline(unit(df$x[i] - dx, "in"), gp = gpar(col = dashed, lty = "dashed"))
    }
    invisible(NULL)
}
