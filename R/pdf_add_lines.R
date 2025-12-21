#' Add lines to a pdf
#'
#' `pdf_add_lines()` adds lines along the components of a print-and-play layout.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_add_crosshairs
#' @param gp Passed to [grid::grid.segments()].
#' @param ... Ignored for now.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates pdf file with added line segments.
#' @seealso [grid_add_lines()], [pdf_add_rects()]
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter")
#' f2 <- pdf_add_lines(f1, layout = "poker_3x3", dpi = 75)
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_add_lines <- function(
	input,
	output = NULL,
	...,
	layout = "poker_3x3",
	pages = "all",
	dpi = 300,
	gp = gpar()
) {
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

		raster <- pdf_render_raster(input, page = i, dpi = dpi)
		grid.raster(raster, interpolate = FALSE, vp = vp)

		if (i %in% pages) {
			grid_add_lines(..., layout = layout, gp = gp)
		}
	}
	invisible(dev.off())
	invisible(output)
}

draw_hline <- function(y = unit(0.5, "npc"), ...) {
	grid.segments(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = y, y1 = y, ...)
}
draw_vline <- function(x = unit(0.5, "npc"), ...) {
	grid.segments(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = x, x1 = x, ...)
}

#' Draw lines along component edges
#'
#' `grid_add_lines()` draws lines along the components of a print-and-play layout.
#'
#' * This function draws in **inches** so make sure your graphics device is "big" enough.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_add_crosshairs
#' @param gp Passed to [grid::grid.segments()].
#' @param ... Ignored for now.
#' @return `NULL` invisibly.
#'         As a side effect draws rectangles to the active graphics device.
#' @seealso [grid::grid.segments()]
#' @examples
#' grid::grid.newpage()
#' vp <- grid::viewport(width=8.5, height=11, default.units="in",
#'                      x=0.0, y=0.0, just=c("left", "bottom"))
#' grid::pushViewport(vp)
#' grid_add_lines(layout = "poker_3x3",
#'                gp = grid::gpar(lty = "dashed", col = "grey"))
#' grid::popViewport()
#' @export
grid_add_lines <- function(..., layout = "poker_3x3", gp = gpar()) {
	chkDots(...)
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}

	for (i in seq_len(nrow(layout))) {
		dy <- 0.5 * (layout$height[i])
		dx <- 0.5 * (layout$width[i])
		draw_hline(unit(layout$y[i] + dy, "in"), gp = gp)
		draw_hline(unit(layout$y[i] - dy, "in"), gp = gp)
		draw_vline(unit(layout$x[i] + dx, "in"), gp = gp)
		draw_vline(unit(layout$x[i] - dx, "in"), gp = gp)
	}
	invisible(NULL)
}
