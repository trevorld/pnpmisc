#' Scale pdf contents centered within the page
#'
#' `pdf_scale()` scales the contents of a pdf centered within the page.
#'
#' * The original pdf document will be rasterized.
#'   See [pdf_apply()] for details.
#'
#' @inheritParams pdf_apply
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a scaled pdf file.
#' @examples
#' input <- pdf_create_blank(paper = "letter", bg = "blue")
#' output <- pdf_scale(input, scale = 0.9, dpi = 75)
#' pdf_width(output)
#' pdf_height(output)
#' unlink(input)
#' unlink(output)
#' @export
pdf_scale <- function(
	input,
	output = NULL,
	...,
	scale,
	bg = "white",
	rasterize = rasterise,
	dpi = getOption("pnpmisc.dpi", 300),
	rasterise = NULL
) {
	chkDots(...)
	pdf_apply(
		input,
		output,
		rasterize = rasterize,
		dpi = dpi,
		bg = bg,
		scale = scale
	)
}
