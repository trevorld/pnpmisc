#' Crop out a component from a print-and-play layout
#'
#' `bm_crop_layout()` crops out a print-and-play component from a layout.
#' @param page A [bittermelon::bm_pixmap()] object representing a print-and-play layout page.
#'             Often the output from [pdf_render_bm_pixmap()].
#' @param ... Ignored for now.
#' @param layout Either a layout preset name in [layout_names()] or a data frame
#'               with layout data (as returned by [layout_grid()]).
#' @param row,col The `row` and `col` of the component in the layout (integers).
#' @param bleed Include the bleed (if available).
#' @return A [bittermelon::bm_pixmap()] object.
#' @examples
#' \dontrun{# User not expected to have this PDF file
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   input <- "A Nice Cuppa - PNP.pdf"
#'   page <- pdf_render_bm_pixmap(input, page = 4L, dpi = 75)
#'   card <- bm_crop_layout(page, layout = "button_shy_cards", row = 1L, col = 1L)
#'   grid::grid.raster(card)
#' }
#' }
#' @export
bm_crop_layout <- function(
	page,
	...,
	layout = "button_shy_cards",
	row = 1L,
	col = 1L,
	bleed = FALSE
) {
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))
	stopifnot(bittermelon::is_bm_pixmap(page))
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	rows <- bm_card_rows(page, layout = layout, row = row, col = col, bleed = bleed)
	cols <- bm_card_cols(page, layout = layout, row = row, col = col, bleed = bleed)
	page[rows, cols]
}

#### Also bleed
# Get card from page
bm_card_rows <- function(page, ..., layout, row = 1L, col = 1L, bleed = FALSE) {
	i <- which(layout$row == row & layout$col == col)
	dpi <- get_dpi(page, layout$paper[i], layout$orientation[i])
	if (bleed) {
		height <- layout$height[i] + 2 * layout$bleed[i]
	} else {
		height <- layout$height[i]
	}
	rows <- seq.int(
		from = dpi * (layout$y[i] - 0.5 * height),
		to = dpi * (layout$y[i] + 0.5 * height)
	)
	rows
}

# Get card from page
bm_card_cols <- function(page, ..., layout, row = 1L, col = 1L, bleed = FALSE) {
	i <- which(layout$row == row & layout$col == col)
	dpi <- get_dpi(page, layout$paper[i], layout$orientation[i])
	if (bleed) {
		width <- layout$width[i] + 2 * layout$bleed[i]
	} else {
		width <- layout$width[i]
	}
	cols <- seq.int(
		from = dpi * (layout$x[i] - 0.5 * width),
		to = dpi * (layout$x[i] + 0.5 * width)
	)
	cols
}

get_dpi <- function(page, paper, orientation) {
	width_in <- paper_width(paper, orientation)
	ncol(page) / width_in
}
