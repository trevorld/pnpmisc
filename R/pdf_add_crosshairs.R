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
	stopifnot(requireNamespace("piecepackr", quietly = TRUE))
	stopifnot(packageVersion("piecepackr") >= "1.14.0-5")
	current_dev <- dev.cur()

	pages <- pdf_pages(input, pages = pages)

	output <- normalize_output(output, input)
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}

	df_size_orig <- pdftools::pdf_pagesize(input)
	stopifnot(nrow(df_size_orig) > 0L)
	width <- unit(df_size_orig$width[1L], "bigpts")
	height <- unit(df_size_orig$height[1L], "bigpts")
	width_in <- convertWidth(width, "inches", valueOnly = TRUE)
	height_in <- convertHeight(height, "inches", valueOnly = TRUE)

	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	} else {
		invisible(dev.off())
	} # `convertWidth()` opened device

	pnp_pdf(output, width = width_in, height = height_in)
	for (i in seq_len(nrow(df_size_orig))) {
		grid.newpage()

		width <- unit(df_size_orig$width[i], "bigpts")
		height <- unit(df_size_orig$height[i], "bigpts")
		vp <- viewport(width = width, height = height)

		r <- pdf_render_raster(input, page = i, dpi = dpi)
		grid.raster(r, interpolate = FALSE, vp = vp)

		if (i %in% pages) {
			grid_add_crosshairs(..., layout = layout)
		}
	}
	invisible(dev.off())
	invisible(output)
}

#' Draw (rounded) rectangles around components
#'
#' `grid_add_rects()` draws (rounded) rectangles around components of a print-and-play layout.
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
