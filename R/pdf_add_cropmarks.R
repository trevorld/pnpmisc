#' Add crop marks to a pdf
#'
#' `pdf_add_cropmarks()` adds crop marks to the edges of components of a print-and-play layout.
#'
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @inheritParams bm_crop_layout
#' @inheritParams piecepackr::grid.cropmark
#' @param bleed Passed to [piecepackr::grid.cropmark()].  
#'              If `NULL` defaults to `max(max(layout$bleed), 0.125)`.
#' @param ... Passed to [piecepackr::grid.cropmark()].
#' @return `output` pdf file name invisibly.
#'         As a side effect adds crop marks to a pdf.
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE)) {
#'   input <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
#'   output <- pdf_add_cropmarks(input, pages = "odd",
#'                               layout = "button_shy_cards", dpi = 75)
#'   unlink(input)
#'   unlink(output)
#' }
#' @export
pdf_add_cropmarks <- function(input, output = NULL, ...,
                              layout = "poker_3x3",
                              pages = "even",
                              dpi = 300, 
                              bleed = NULL) {
    stopifnot(requireNamespace("piecepackr", quietly = TRUE))
    current_dev <- dev.cur()

    pages <- pdf_pages(input, pages = pages)

    output <- normalize_output(output, input)
    if (is.character(layout))
        layout <- layout_preset(layout)
    bleed <- bleed %||% max(max(layout$bleed), 0.125)
    layout$bleed <- NULL

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
            i_left <- which(layout$x - layout$width == min(layout$x - layout$width))
            piecepackr::pmap_piece(layout[i_left, ], piecepackr::grid.cropmark,
                    default.units = "in", cm_select = "67", bleed = bleed, ...)

            i_right <- which(layout$x + layout$width == max(layout$x + layout$width))
            piecepackr::pmap_piece(layout[i_right, ], piecepackr::grid.cropmark,
                    default.units = "in", cm_select = "23", bleed = bleed, ...)

            i_top <- which(layout$y + layout$height == max(layout$y + layout$height))
            piecepackr::pmap_piece(layout[i_top, ], piecepackr::grid.cropmark,
                    default.units = "in", cm_select = "18", bleed = bleed, ...)

            i_bot <- which(layout$y - layout$height == min(layout$y - layout$height))
            piecepackr::pmap_piece(layout[i_bot, ], piecepackr::grid.cropmark,
                    default.units = "in", cm_select = "45", bleed = bleed, ...)
        }
    }
    invisible(dev.off())
    invisible(output)
}
