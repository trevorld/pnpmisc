#' Pad pdf file (to a larger size)
#'
#' `pdf_pad_pagesize()` makes a pdf file larger by padding it (i.e. adding space to the outside margins).
#' The original images are **not** rescaled.
#' @param input Input pdf filename.
#' @param output Output pdf filename.  `NULL` defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Ignored.
#' @param bg `output` pdf background color.  Passed to [grDevices::pdf()].
#' @param dpi Dots per inch.  Passed to [pdftools::pdf_render_page()].
#' @param paper_size Paper size to extend to.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates padded pdf file.
#' @examples
#' # Some PnP files' size is the intersection of A4/letter page sizes
#' # i.e. shorter than A4 and narrower than letter.
#' # We usually want pad to full A4 or letter page size.
#' input <- tempfile(fileext = ".pdf")
#' output_letter <- tempfile(fileext = "_letter.pdf")
#' output_a4 <- tempfile(fileext = "_a4.pdf")
#' grDevices::pdf(input, width = 8.3, height = 11, bg = "blue")
#' grid::grid.text("")
#' invisible(grDevices::dev.off())
#'
#' pdf_width(input)
#' pdf_height(input)
#'
#' output <- pdf_pad_pagesize(input)
#' pdf_width(output)
#' pdf_height(output)
#' unlink(output)
#'
#' output_a4 <- pdf_pad_pagesize(input, paper_size = "A4")
#' pdf_width(output_a4)
#' pdf_height(output_a4)
#' unlink(output_a4)
#'
#' unlink(input)
#' @export
pdf_pad_pagesize <- function(input, output = NULL, ...,
                    bg = "white", dpi = 300,
                    paper_size = c("letter", "A4")) {
    paper_size <- match.arg(paper_size)
    output <- normalize_output(output, input)

    current_dev <- dev.cur()
    if (current_dev > 1) on.exit(dev.set(current_dev), add = TRUE)

    df_size_orig <- pdftools::pdf_pagesize(input)
    stopifnot(nrow(df_size_orig) > 0L)
    orientation <- ifelse(df_size_orig$width[1L] > df_size_orig$height[1L],
                          "landscape", "portrait")

    pdf2(output, paper_size = paper_size, orientation = orientation, bg = bg)
    for (i in seq_len(nrow(df_size_orig))) {
        width <- unit(df_size_orig$width[i], "bigpts")
        height <- unit(df_size_orig$height[i], "bigpts")
        vp <- viewport(width = width, height = height)
        grid.newpage()
        bitmap <- pdftools::pdf_render_page(input, page = i, dpi = dpi, numeric = TRUE)
        pushViewport(vp)
        grid.raster(bitmap, interpolate = FALSE)
        popViewport()
    }
    invisible(dev.off())
    invisible(output)
}

pdf2 <- function(output, ...,
                 paper_size = c("letter", "A4"),
                 orientation = c("portrait", "landscape")) {
    paper_size <- match.arg(paper_size)
    orientation <- match.arg(orientation)

    pg_width <- switch(paper_size, letter = LETTER_WIDTH, A4 = A4_WIDTH)
    pg_height <- switch(paper_size, letter = LETTER_HEIGHT, A4 = A4_HEIGHT)

    if (orientation == "landscape") { # landscape
        width <- pg_height
        height <- pg_width
    } else { # portrait
        width <- pg_width
        height <- pg_height
    }
    pdf(output, width = width, height = height, ...)
}
