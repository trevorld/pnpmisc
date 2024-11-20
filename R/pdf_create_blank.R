#' Create blank pdf pages
#'
#' `pdf_create_blank()` creates blank pdf pages.
#'
#' @inheritParams pdf_pad_paper
#' @param length Number of pages to create.
#' @param width,height Paper size in inches if `paper = "special"`.
#' @param orientation Either "portrait" or "landscape".  Ignored if `paper = "special"`.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a blank pdf file.
#' @examples
#' f <- pdf_create_blank(length = 4L)
#' unlink(f) # clean up
#' @export
pdf_create_blank <- function(output = NULL, ...,
                             length = 1L,
                             paper = c("special", "letter", "a4"),
                             orientation = c("portrait", "landscape"),
                             width = 8.5, height = 11, bg = "white") {
    paper <- tolower(paper)
    paper <- match.arg(paper)
    orientation <- match.arg(orientation)
    output <- normalize_output(output)

    current_dev <- dev.cur()
    if (current_dev > 1) on.exit(dev.set(current_dev), add = TRUE)

    pnp_pdf(output, paper = paper, orientation = orientation,
            width = width, height = height, bg = bg)
    for (page in seq.int(length))
        grid.newpage()
    invisible(dev.off())

    invisible(output)
}
