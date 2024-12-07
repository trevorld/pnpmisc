#' Add crosshairs to a pdf
#'
#' `pdf_add_crosshairs()` adds crosshairs to the corners of components of a print-and-play layout.
#'
#' * The default layout supports Button Shy games.
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @inheritParams bm_crop_layout
#' @param ... Passed to [piecepackr::grid.crosshair()].
#' @return `output` pdf file name invisibly.
#'         As a side effect adds crosshairs to a pdf.
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-5") {
#'   input <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
#'   output <- pdf_add_crosshairs(input, pages = "odd",
#'                                layout = "button_shy_cards", dpi = 75)
#'   unlink(input)
#'   unlink(output)
#' }
#' @export
pdf_add_crosshairs <- function(input, output = NULL, ...,
                              layout = "button_shy_cards",
                              pages = "even",
                              dpi = 300) {
    stopifnot(requireNamespace("piecepackr", quietly = TRUE))
    stopifnot(packageVersion("piecepackr") >= "1.14.0-5")
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

        r <- pdf_render_raster(input, page = i, dpi = dpi)
        grid.raster(r, interpolate = FALSE, vp = vp)

        pixmap <- pdf_render_bm_pixmap(input, page = i, dpi = dpi)
        if (i %in% pages) {
            for (j in seq_len(nrow(layout))) {
                piecepackr::grid.crosshair(
                    x = layout$x[j], y = layout$y[j],
                    width = layout$width[j], height = layout$height[j],
                    default.units = "in", ...
                )
            }
        }
    }
    invisible(dev.off())
    invisible(output)
}
