#' Overlay custom graphics on a pdf
#'
#' `pdf_add_overlay()` renders each page of a pdf as a raster image and calls
#' `fn` on selected pages to draw custom graphics on top.
#'
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @param grid_fn A zero-argument function called to draw graphics on selected pages.
#'               Defaults to [grid::grid.null()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a pdf with overlaid graphics.
#' @seealso [pdf_add_cropmarks()], [pdf_add_crosshairs()],
#'          [pdf_add_lines()], [pdf_add_rects()]
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
	dpi = 300,
	grid_fn = grid::grid.null
) {
	chkDots(...)
	current_dev <- dev.cur()
	pages <- pdf_pages(input, pages = pages)
	output <- normalize_output(output, input)

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
			grid_fn()
		}
	}
	invisible(dev.off())
	invisible(output)
}
