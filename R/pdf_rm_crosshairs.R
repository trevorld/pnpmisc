#' Remove crosshairs
#'
#' `pdf_rm_crosshairs()` removes unwanted crosshairs.
#' Currently only supports [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP letter size v1).
#'
#' @inheritParams pdf_pad
#' @inheritParams pdf_pages
#' @return `output` pdf file name invisibly.
#'         As a side effect removes from crosshairs from a pdf.
#' @export
pdf_rm_crosshairs <- function(input, output = NULL, ..., pages = "odd") {
    stopifnot(requireNamespace("bittermelon", quietly = TRUE))
    current_dev <- dev.cur()

    pages <- pdf_pages(input, pages = pages)

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

    pdf(output, width = width_in, height = height_in)
    for (i in seq_len(nrow(df_size_orig))) {
        width <- unit(df_size_orig$width[i], "bigpts")
        height <- unit(df_size_orig$height[i], "bigpts")
        vp <- viewport(width = width, height = height)

        grid.newpage()
        bitmap <- pdftools::pdf_render_page(input, page = i, dpi = 300, numeric = TRUE)
        pixmap <- bittermelon::as_bm_pixmap(bitmap)

        if (i %in% pages) {
            pixmap <- pdf_rm_crosshairs_galdors_grip(pixmap, page = i)
        }

        pushViewport(vp)
        grid::grid.raster(pixmap, interpolate = FALSE)
        popViewport()
    }
    invisible(dev.off())
    invisible(output)
}

# Use `bittermelon::bm_pixel_picker()` to help figure these out
# array = pdftools::pdf_render_page(input, page = 11, numeric = T, dpi = 300)
# pm = bittermelon::as_bm_pixmap(array)
# bittermelon::bm_pixel_picker(pm)
pdf_rm_crosshairs_galdors_grip <- function(pixmap, page) {
    bg_light <- "#F8F6E7FF"
    left <- 422L
    midright <- 2063L
    right <- 2875L
    low1 <- 162L
    low2 <- 215L
    mid1 <- 1214L
    mid2 <- 1274L
    mid3 <- 1338L
    high1 <- 2336L
    high2 <- 2391L
    if (is_odd(page)) {
        pixmap[low1:low2, left:right] <- bg_light
        pixmap[mid1:mid3, left:right] <- bg_light
        pixmap[high1:high2, left:right] <- bg_light
        if (page == 11L) {
            pixmap[low1:low2, midright:right] <- "black"
            pixmap[mid1:mid2, midright:right] <- "black"
        }
    } else {
        pixmap[low1:low2, left:right] <- "black"
        pixmap[mid1:mid3, left:right] <- "black"
        pixmap[high1:high2, left:right] <- "black"
    }
    pixmap
}
