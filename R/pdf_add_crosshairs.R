#' Add crosshairs to a pdf
#'
#' `pdf_add_crosshairs()` adds crosshairs to the corners of components of a print-and-play layout.
#'
#' * The default layout supports Button Shy games.
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @inheritParams bm_crop_layout
#' @param ... Passed to [piecepackr::grid.crosshair()].
#' @return `output` pdf file name invisibly.
#'         As a side effect adds crosshairs to a pdf.
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-5") {
#'   input <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
#'   output <- pdf_add_crosshairs(input, pages = "odd",
#'                                layout = "button_shy_cards", dpi = 75)
#'   unlink(input)
#'   unlink(output)
#' }
#' @export
pdf_add_crosshairs <- function(
	input,
	output = NULL,
	...,
	layout = "button_shy_cards",
	pages = "even",
	dpi = 300
) {
	pdf_add_overlay(input, output, pages = pages, dpi = dpi, grid_fn = \() {
		grid_add_crosshairs(..., layout = layout)
	})
}

#' Draw crosshairs at component corners
#'
#' `grid_add_crosshairs()` draws crosshairs at the corners of components of a print-and-play layout.
#'
#' * This function draws in **inches** so make sure your graphics device is "big" enough.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @inheritParams bm_crop_layout
#' @param ... Passed to [piecepackr::grid.crosshair()].
#' @return `NULL` invisibly.
#'         As a side effect draws crosshairs to the active graphics device.
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-5") {
#'   grid::grid.newpage()
#'   vp <- grid::viewport(width=8.5, height=11, default.units="in",
#'                        x=0.5, y=0.5, just=c("left", "bottom"))
#'   grid::pushViewport(vp)
#'   grid_add_crosshairs(layout = "poker_3x3")
#'   grid::popViewport()
#' }
#' @export
grid_add_crosshairs <- function(..., layout = "poker_3x3") {
	stopifnot(requireNamespace("piecepackr", quietly = TRUE))
	stopifnot(packageVersion("piecepackr") >= "1.14.0-5")
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	for (j in seq_len(nrow(layout))) {
		piecepackr::grid.crosshair(
			x = layout$x[j],
			y = layout$y[j],
			width = layout$width[j],
			height = layout$height[j],
			default.units = "in",
			...
		)
	}
	invisible(NULL)
}
