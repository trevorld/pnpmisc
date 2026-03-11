#' Add crop marks to a pdf
#'
#' `pdf_add_cropmarks()` adds crop marks to the edges of components of a print-and-play layout.
#'
#' @inheritParams pdf_add_crosshairs
#' @param gp,cm_width,cm_length Passed to [piecepackr::grid.cropmark()].
#' @param ... Passed to [pdf_add_overlay()].
#' @return `output` pdf file name invisibly.
#'         As a side effect adds crop marks to a pdf.
#' @seealso [grid_add_cropmarks()], [piecepackr::grid.cropmark()]
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE)) {
#'   input <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
#'   output <- pdf_add_cropmarks(input, pages = "odd",
#'                               layout = "button_shy_cards", dpi = 75)
#'   unlink(input)
#'   unlink(output)
#' }
#' @export
pdf_add_cropmarks <- function(
	input,
	output = NULL,
	...,
	layout = "poker_3x3",
	gp = gpar(),
	cm_width = unit(0.25, "mm"),
	cm_length = unit(0.125, "in")
) {
	pdf_add_overlay(
		input,
		output,
		...,
		grid_fn = \() {
			grid_add_cropmarks(layout = layout, gp = gp, cm_width = cm_width, cm_length = cm_length)
		}
	)
}

#' Draw crop marks around components
#'
#' `grid_add_cropmarks()` adds crop marks to the edges of components of a print-and-play layout.
#'
#' * This function draws in **inches** so make sure your graphics device is "big" enough.
#'
#' @inheritParams bm_crop_layout
#' @param gp,cm_width,cm_length Passed to [piecepackr::grid.cropmark()].
#' @param bleed Passed to [piecepackr::grid.cropmark()].
#'              If `NULL` defaults to `max(max(layout$bleed), 0.125)`.
#' @param ... Passed to [piecepackr::grid.cropmark()].
#' @return `NULL` invisibly.
#'         As a side effect draws crop marks to the active graphics device.
#' @seealso [pdf_add_cropmarks()], [piecepackr::grid.cropmark()]
#' @examples
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-6") {
#'   grid::grid.newpage()
#'   vp <- grid::viewport(width=8.5, height=11, default.units="in",
#'                        x=0.5, y=0.5, just=c("left", "bottom"))
#'   grid::pushViewport(vp)
#'   grid_add_rects(layout = "poker_3x3")
#'   grid_add_cropmarks(layout = "poker_3x3")
#'   grid::popViewport()
#' }
#' @export
grid_add_cropmarks <- function(
	...,
	layout = "poker_3x3",
	bleed = NULL,
	gp = gpar(),
	cm_width = unit(0.25, "mm"),
	cm_length = unit(0.125, "in")
) {
	stopifnot(requireNamespace("piecepackr", quietly = TRUE))
	stopifnot(packageVersion("piecepackr") >= "1.14.0-6")
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	bleed <- bleed %||% max(max(layout$bleed), 0.125)
	if (is.unit(bleed)) {
		bleed <- convertUnit(bleed, "in", valueOnly = TRUE)
	}

	i_left <- which(layout$x - layout$width == min(layout$x - layout$width))
	piecepackr::grid.cropmark(
		x = layout$x[i_left],
		y = layout$y[i_left],
		width = layout$width[i_left],
		height = layout$height[i_left],
		default.units = "in",
		cm_select = "67",
		bleed = bleed,
		gp = gp,
		cm_width = cm_width,
		cm_length = cm_length,
		...
	)

	i_right <- which(layout$x + layout$width == max(layout$x + layout$width))
	piecepackr::grid.cropmark(
		x = layout$x[i_right],
		y = layout$y[i_right],
		width = layout$width[i_right],
		height = layout$height[i_right],
		default.units = "in",
		cm_select = "23",
		bleed = bleed,
		gp = gp,
		cm_width = cm_width,
		cm_length = cm_length,
		...
	)

	i_top <- which(layout$y + layout$height == max(layout$y + layout$height))
	piecepackr::grid.cropmark(
		x = layout$x[i_top],
		y = layout$y[i_top],
		width = layout$width[i_top],
		height = layout$height[i_top],
		default.units = "in",
		cm_select = "18",
		bleed = bleed,
		gp = gp,
		cm_width = cm_width,
		cm_length = cm_length,
		...
	)

	i_bot <- which(layout$y - layout$height == min(layout$y - layout$height))
	piecepackr::grid.cropmark(
		x = layout$x[i_bot],
		y = layout$y[i_bot],
		width = layout$width[i_bot],
		height = layout$height[i_bot],
		default.units = "in",
		cm_select = "45",
		bleed = bleed,
		gp = gp,
		cm_width = cm_width,
		cm_length = cm_length,
		...
	)

	invisible(NULL)
}
