#' Overlay custom graphics on a pdf
#'
#' `pdf_add_overlay()` calls `grid_fn` on selected pages to draw custom
#' graphics on top.
#'
#' @inheritParams pdf_apply
#' @param grid_fn A zero-argument function called to draw graphics on selected pages.
#'               Defaults to [grid::grid.null()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a pdf with overlaid graphics.
#' @seealso [pdf_apply()] which this function wraps and
#'          [pdf_add_cropmarks()], [pdf_add_crosshairs()],
#'          [pdf_add_lines()], and [pdf_add_rects()] for specific applications
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter")
#' f2 <- pdf_add_overlay(f1, pages = "all", dpi = 75,
#'                       grid_fn = function() {
#'                         grid::grid.text("DRAFT", gp = grid::gpar(col = "red",
#'                                         fontsize = 72, alpha = 0.3))
#'                       })
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_add_overlay <- function(
	input,
	output = NULL,
	...,
	pages = "all",
	rasterize = rasterise,
	dpi = getOption("pnpmisc.dpi", 300),
	paper = NULL,
	grid_fn = grid::grid.null,
	rasterise = NULL
) {
	chkDots(...)
	pdf_apply(
		input,
		output,
		pages = pages,
		rasterize = rasterize,
		dpi = dpi,
		paper = paper,
		grid_fn = grid_fn
	)
}
