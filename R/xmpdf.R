#' Wrappers around xmpdf functions
#'
#' These functions wrap around the utilities in the xmpdf package
#' but alters the arguments to match this package's conventions:
#' the first two arguments must be `input` and `output`,
#' by default `output = tempfile(fileext = ".pdf")`, and all other
#' arguments must be named.
#' @inheritParams pdf_pad_paper
#' @param ... Currently ignored.
#' @return `output` filename of new pdf file invisibly.
#' @seealso [xmpdf::set_bookmarks()], [xmpdf::set_docinfo()], [xmpdf::set_xmp()]
#' @examples
#' f1 <- pdf_create_blank(length = 2L)
#'
#' if (requireNamespace("xmpdf", quietly = TRUE) &&
#'     xmpdf::supports_set_bookmarks()) {
#'   bm <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
#'   f2 <- pdf_set_bookmarks(f1, bookmarks = bm)
#'   unlink(f2)
#' }
#'
#' if (requireNamespace("xmpdf", quietly = TRUE) &&
#'     xmpdf::supports_set_docinfo()) {
#'   di <- xmpdf::docinfo(title = "A Title", author = "The Author")
#'   f3 <- pdf_set_docinfo(f1, docinfo = di)
#'   unlink(f3)
#' }
#'
#' if (requireNamespace("xmpdf", quietly = TRUE) &&
#'     xmpdf::supports_set_xmp()) {
#'   xmp <- xmpdf::xmp(title = "A Title", creator = "The Author")
#'   f4 <- pdf_set_xmp(f1, xmp = xmp)
#'   unlink(f4)
#' }
#' unlink(f1)
#' @param bookmarks See [xmpdf::set_bookmarks()].
#' @rdname xmpdf_wrappers
#' @export
pdf_set_bookmarks <- function(input, output = NULL, ..., bookmarks) {
    stopifnot(requireNamespace("xmpdf", quietly = TRUE))
    output <- normalize_output(output, input)
    xmpdf::set_bookmarks(bookmarks, input, output)
    invisible(output)
}

#' @param docinfo See [xmpdf::set_docinfo()].
#' @rdname xmpdf_wrappers
#' @export
pdf_set_docinfo <- function(input, output = NULL, ..., docinfo) {
    stopifnot(requireNamespace("xmpdf", quietly = TRUE))
    output <- normalize_output(output, input)
    xmpdf::set_docinfo(docinfo, input, output)
    invisible(output)
}

#' @param xmp See [xmpdf::set_xmp()].
#' @rdname xmpdf_wrappers
#' @export
pdf_set_xmp <- function(input, output = NULL, ..., xmp) {
    stopifnot(requireNamespace("xmpdf", quietly = TRUE))
    output <- normalize_output(output, input)
    xmpdf::set_xmp(xmp, input, output)
    invisible(output)
}
