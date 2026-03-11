#' Rasterize a pdf
#'
#' `pdf_rasterize()` re-renders a pdf by rasterizing each page.
#'
#' @inheritParams pdf_apply
#' @param ... Passed to [pdf_apply()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a rasterized pdf.
#' @seealso [pdf_apply()] which this function wraps.
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter")
#' f2 <- pdf_rasterize(f1, dpi = 75)
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_rasterize <- function(
	input,
	output = NULL,
	...,
	dpi = getOption("pnpmisc.dpi", 300)
) {
	pdf_apply(input, output, ..., rasterize = TRUE, dpi = dpi)
}
