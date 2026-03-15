#' Wrappers around `qpdf` functions
#'
#' These functions wrap around the utilities in the `qpdf` package
#' but alters the arguments to match this package's conventions:
#' the first two arguments must be `input` and `output` (which by default `tempfile(fileext = ".pdf")`),
#' all other arguments must be named,
#' and any `pages` argument is first processed by [pdf_pages()] (so you can also use strings like "even" or "odd").
#' @inheritParams pdf_apply
#' @param input For `pdf_combine()`, a character vector of input pdf filenames.
#'   For all other functions, a single input pdf filename.
#' @param ... Passed to the underlying qpdf functions.
#' @rdname qpdf_wrappers
#' @return `output` filename of new pdf file invisibly.
#' @seealso [qpdf::pdf_combine()], [qpdf::pdf_compress()],
#'   [qpdf::pdf_overlay_stamp()], [qpdf::pdf_rotate_pages()], [qpdf::pdf_subset()]
#' @examples
#' f1 <- pdf_create_blank(width = 6, height = 4)
#' f2 <- pdf_create_blank(width = 4, height = 6)
#' f3 <- pdf_combine(c(f1, f2))
#' f4 <- pdf_overlay_stamp(f3, stamp = f1)
#' f5 <- pdf_compress(f4)
#' f6 <- pdf_subset(f5, pages = 1L)
#' f7 <- pdf_rotate_pages(f6, angle = 90)
#'
#' unlink(f1)
#' unlink(f2)
#' unlink(f3)
#' unlink(f4)
#' unlink(f5)
#' unlink(f6)
#' unlink(f7)
#' @export
pdf_combine <- function(input, output = NULL, ...) {
	output <- normalize_output(output)
	qpdf::pdf_combine(input, output = output, ...)
	invisible(output)
}

#' @rdname qpdf_wrappers
#' @export
pdf_compress <- function(input, output = NULL, ...) {
	output <- normalize_output(output, input)
	qpdf::pdf_compress(input, output = output, ...)
	invisible(output)
}

#' @rdname qpdf_wrappers
#' @export
pdf_overlay_stamp <- function(input, output = NULL, ...) {
	output <- normalize_output(output, input)
	qpdf::pdf_overlay_stamp(input, output = output, ...)
	invisible(output)
}

#' @inheritParams pdf_apply
#' @rdname qpdf_wrappers
#' @export
pdf_rotate_pages <- function(input, output = NULL, ..., pages = "all") {
	output <- normalize_output(output, input)
	pages <- pdf_pages(input, pages = pages)
	qpdf::pdf_rotate_pages(input, output = output, ..., pages = pages)
	invisible(output)
}

#' @inheritParams pdf_apply
#' @rdname qpdf_wrappers
#' @export
pdf_subset <- function(input, output = NULL, ..., pages = 1L) {
	output <- normalize_output(output, input)
	pages <- pdf_pages(input, pages = pages)
	qpdf::pdf_subset(input, output = output, ..., pages = pages)
	invisible(output)
}
