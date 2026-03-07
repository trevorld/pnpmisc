#' Rasterize a pdf
#'
#' `pdf_rasterize()` re-renders a pdf by rasterizing each page.
#'
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a rasterized pdf.
#' @seealso [pdf_apply()] which this function wraps.
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter")
#' f2 <- pdf_rasterize(f1, dpi = 75)
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_rasterize <- function(input, output = NULL, ..., dpi = 300) {
	chkDots(...)
	pdf_apply(input, output, dpi = dpi)
}
