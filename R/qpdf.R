#' Wrappers around qpdf functions
#'
#' These functions wrap around the utilities in the qpdf package
#' but alters the arguments to match this package's conventions:
#' the first two arguments must be `input` and `output`,
#' by default `output = tempfile(fileext = ".pdf")`, and all other
#' arguments must be named.
#' @inheritParams pdf_pad
#' @param ... Passed to the underlying qpdf functions.
#' @rdname qpdf_wrappers
#' @return `output` filename of new pdf file invisibly.
#' @seealso [qpdf::pdf_compress()], [qpdf::pdf_rotate_pages()], [qpdf::pdf_subset()]
#' @examples
#' f1 <- tempfile(fileext = ".pdf")
#' pdf(f1, width = 6, height = 4)
#' grid::grid.text("")
#' invisible(dev.off())
#'
#' f2 <- pdf_compress(f1)
#' f3 <- pdf_subset(f2, pages = 1L)
#' f4 <- pdf_rotate_pages(f3, angle = 90)
#'
#' unlink(f1)
#' unlink(f2)
#' unlink(f3)
#' unlink(f4)
#' @export
pdf_compress <- function(input, output = NULL, ...) {
    output <- normalize_output(output, input)
    qpdf::pdf_compress(input, output = output, ...)
    invisible(output)
}

#' @inheritParams pdf_pages
#' @rdname qpdf_wrappers
#' @export
pdf_rotate_pages <- function(input, output = NULL, ..., pages = "all") {
    output <- normalize_output(output, input)
    pages <- pdf_pages(input, pages = pages)
    qpdf::pdf_rotate_pages(input, output = output, ..., pages = pages)
    invisible(output)
}

#' @rdname qpdf_wrappers
#' @export
pdf_subset <- function(input, output = NULL, ..., pages = 1L) {
    output <- normalize_output(output, input)
    pages <- pdf_pages(input, pages = pages)
    qpdf::pdf_subset(input, output = output, ..., pages = pages)
    invisible(output)
}
