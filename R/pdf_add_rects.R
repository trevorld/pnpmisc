#' Add (round)rects to a pdf
#'
#' `pdf_add_rects()` adds (round)rects around components of a print-and-play layout.
#'
#' * Sometimes if you use the same color as a solid background color
#'   this can be used to effectively "remove" unwanted card outlines.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_add_crosshairs
#' @param r,gp Passed to [grid::grid.roundrect()].
#' @param ... Ignored for now.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates pdf file with added origami symbols.
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter")
#' f2 <- pdf_add_rects(f1, layout = "poker_3x3", dpi = 75)
#' # "Remove" unwanted card border lines by covering up with white
#' f3 <- pdf_add_rects(f2, layout = "poker_3x3", dpi = 75, 
#'                     gp = grid::gpar(col = "white", fill = NA, lwd = 2))
#' unlink(f1)
#' unlink(f2)
#' unlink(f3)
#' @export
pdf_add_rects <- function(input, output = NULL, ...,
                          layout = "poker_3x3",
                          pages = "all", dpi = 300,
                          r = unit(0, "in"),
                          gp = gpar(col = "black", fill = NA, lwd = 1)) {
    current_dev <- dev.cur()

    pages <- pdf_pages(input, pages = pages)

    output <- normalize_output(output, input)
    if (is.character(layout))
        layout <- layout_preset(layout)

    df_size_orig <- pdftools::pdf_pagesize(input)
    stopifnot(nrow(df_size_orig) > 0L)
    width <- unit(df_size_orig$width[1L], "bigpts")
    height <- unit(df_size_orig$height[1L], "bigpts")
    width_in <- convertWidth(width, "inches", valueOnly = TRUE)
    height_in <- convertHeight(height, "inches", valueOnly = TRUE)

    if (current_dev > 1)
        on.exit(dev.set(current_dev), add = TRUE)
    else
        invisible(dev.off()) # `convertWidth()` opened device

    pnp_pdf(output, width = width_in, height = height_in)
    for (i in seq_len(nrow(df_size_orig))) {
        grid.newpage()

        width <- unit(df_size_orig$width[i], "bigpts")
        height <- unit(df_size_orig$height[i], "bigpts")
        vp <- viewport(width = width, height = height)

        raster <- pdf_render_raster(input, page = i, dpi = dpi)
        grid.raster(raster, interpolate = FALSE, vp = vp)

        pixmap <- pdf_render_bm_pixmap(input, page = i, dpi = dpi)
        if (i %in% pages) {
            for (j in seq_len(nrow(layout))) {
                grid.roundrect(
                    x = layout$x[j], y = layout$y[j],
                    width = layout$width[j], height = layout$height[j],
                    default.units = "in", r = r, gp = gp
                )
            }
        }
    }
    invisible(dev.off())
    invisible(output)
}
