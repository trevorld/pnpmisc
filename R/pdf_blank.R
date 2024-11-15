#' Create blank pdf pages
#'
#' `pdf_blank()` creates blank pdf pages.
#'
#' @inheritParams pdf_pad_pagesize
#' @param length Number of pages to create.
#' @inheritParams grDevices::pdf
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a blank pdf file.
#' @examples
#' f <- pdf_blank(length = 4L)
#' unlink(f) # clean up
#' @export
pdf_blank <- function(output = NULL, ..., length = 1L, width = 8.5, height = 11, bg = "white") {
    output <- normalize_output(output)

    current_dev <- dev.cur()
    if (current_dev > 1) on.exit(dev.set(current_dev), add = TRUE)

    pdf(output, width = width, height = height, bg = bg)
    for (page in seq.int(length))
        grid.newpage()
    invisible(dev.off())

    invisible(output)
}
