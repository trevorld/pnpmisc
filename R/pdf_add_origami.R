#' Add origami symbols to pdf
#'
#' `pdf_add_origami()` adds origami symbols to the pdf.
#' Currently only supports adding origami symbols to
#' [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home).
#' @inheritParams pdf_pad_paper
#' @return `output` pdf file name invisibly.
#'         As a side effect creates pdf file with added origami symbols.
#' @examples
#' f1 <- pnpmisc:::pdf_create_mock_sbgj()
#' f2 <- pdf_add_origami(f1)
#'
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_add_origami <- function(input, output = NULL, ..., dpi = 300) {
    current_dev <- dev.cur()

    output <- normalize_output(output, input)

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
        width <- unit(df_size_orig$width[i], "bigpts")
        height <- unit(df_size_orig$height[i], "bigpts")
        vp <- viewport(width = width, height = height)

        grid.newpage()
        bitmap <- pdftools::pdf_render_page(input, page = i, dpi = dpi, numeric = TRUE)
        pushViewport(vp)
        grid.raster(bitmap, interpolate = FALSE)
        popViewport()

        xc <- unit(0.5, "npc") - unit(0.7, "mm")
        yc <- unit(0.5, "npc") - unit(1.45, "mm")
        draw_jacket_origami(xc, yc)
    }
    invisible(dev.off())
    invisible(output)
}

draw_jacket_origami <- function(xc = unit(0.5, "npc"), yc = unit(0.5, "npc")) {

    width_fb <- unit(JACKET_FACE_WIDTH, "inches")
    width_s  <- unit(JACKET_SPINE_WIDTH, "inches")
    height   <- unit(JACKET_HEIGHT, "inches")

    # Line up our calculated crop/fold marks with their crop/fold marks
    # piecepackr::grid.cropmark(x = xc, y = yc,
    #     width = 2 * width_fb + width_s, height = height,
    #     bleed = TRUE)
    xo_edge <- width_fb + 0.5 * width_s
    xo_fold <- 0.5 * width_s
    xo_target <- width_fb - 0.5 * width_s
    yo_edge <- 0.5 * height
    yo_dotdash <- yo_edge + unit(2.0, "cm")
    yo_circle <- yo_edge + unit(2.0, "mm")

    # Cover up the "fold here" text
    grid.rect(x = xc, y = yc + yo_edge + unit(1.1, "cm"),
                    width = unit(2, "in"), height = unit(2, "cm"),
                    gp = gpar(col = NA, fill = "white"))
    grid.rect(x = xc, y = yc - yo_edge - unit(1.1, "cm"),
                    width = unit(2, "in"), height = unit(2, "cm"),
                    gp = gpar(col = NA, fill = "white"))

    # Put-the-points-together dots and line
    grid.circle(x = xc - xo_edge, y = yc + yo_circle, r = unit(1, "mm"),
        gp = gpar(col = NA, fill = "black"))
    grid.circle(x = xc + xo_target, y = yc + yo_circle, r = unit(1, "mm"),
        gp = gpar(col = NA, fill = "black"))
    grid.segments(x0 = xc - xo_edge + unit(3, "mm"), x1 = xc + xo_target - unit(3, "mm"),
                  y0 = yc + yo_circle, y1 = yc + yo_circle, gp = gpar(lwd = 2))
    grid.segments(x0 = xc + xo_target - unit(3, "mm"), x1 = xc + xo_target - unit(8, "mm"),
                  y0 = yc + yo_circle, y1 = yc + yo_circle + unit(3, "mm"), gp = gpar(lwd = 2))
    grid.segments(x0 = xc + xo_target - unit(8, "mm"), x1 = xc + xo_target - unit(8, "mm"),
                  y0 = yc + yo_circle + unit(3, "mm"), y1 = yc + yo_circle, gp = gpar(lwd = 2))

    grid.circle(x = xc + xo_edge, y = yc - yo_circle, r = unit(1, "mm"),
        gp = gpar(col = NA, fill = "black"))
    grid.circle(x = xc - xo_target, y = yc - yo_circle, r = unit(1, "mm"),
        gp = gpar(col = NA, fill = "black"))
    grid.segments(x0 = xc + xo_edge - unit(3, "mm"), x1 = xc - xo_target + unit(3, "mm"),
                  y0 = yc - yo_circle, y1 = yc - yo_circle, gp = gpar(lwd = 2))
    grid.segments(x0 = xc - xo_target + unit(3, "mm"), x1 = xc - xo_target + unit(8, "mm"),
                  y0 = yc - yo_circle, y1 = yc - yo_circle - unit(3, "mm"), gp = gpar(lwd = 2))
    grid.segments(x0 = xc - xo_target + unit(8, "mm"), x1 = xc - xo_target + unit(8, "mm"),
                  y0 = yc - yo_circle - unit(3, "mm"), y1 = yc - yo_circle, gp = gpar(lwd = 2))

    # Mount fold dot-dash (or dot-dot-dash)
    grid.segments(x0 = xc - xo_fold, x1 = xc - xo_fold,
                        y0 = yc + yo_edge + unit(0.4, "mm"), y1 = yc + yo_dotdash,
                        gp = gpar(lty = "dotdash", lwd = 2))
    grid.segments(x0 = xc + xo_fold, x1 = xc + xo_fold,
                        y0 = yc + yo_edge + unit(0.4, "mm"), y1 = yc + yo_dotdash,
                        gp = gpar(lty = "dotdash", lwd = 2))
    grid.segments(x0 = xc - xo_fold, x1 = xc - xo_fold,
                        y0 = yc - yo_edge - unit(0.4, "mm"), y1 = yc - yo_dotdash,
                        gp = gpar(lty = "dotdash", lwd = 2))
    grid.segments(x0 = xc + xo_fold, x1 = xc + xo_fold,
                        y0 = yc - yo_edge - unit(0.4, "mm"), y1 = yc - yo_dotdash,
                        gp = gpar(lty = "dotdash", lwd = 2))

}
