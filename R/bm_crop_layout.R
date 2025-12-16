#' Crop out a component from a print-and-play layout
#'
#' `bm_crop_layout()` crops out a print-and-play component from a layout.
#' @param page A [bittermelon::bm_pixmap()] object representing a print-and-play layout page.
#'             Often the output from [pdf_render_bm_pixmap()].
#' @param ... Ignored for now.
#' @param layout Either a layout preset name in [layout_names()] or a data frame
#'               with layout data (as returned by [layout_grid()]).
#' @param row,col The `row` and `col` of the component in the layout (integers).
#' @param name Instead of `row` and `col` can instead use the name of the component in the layout (string).
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
	bleed = FALSE,
	name = NULL
) {
	stopifnot(
		requireNamespace("bittermelon", quietly = TRUE),
		bittermelon::is_bm_pixmap(page),
		is.null(name) || (missing(row) && missing(col))
	)
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	if (is.null(name) || !hasName(layout, "name")) {
		index <- which(layout$row == row & layout$col == col)
	} else {
		index <- which(layout$name == name)
	}
	rows <- bm_card_rows(page, layout = layout, index = index, bleed = bleed)
	cols <- bm_card_cols(page, layout = layout, index = index, bleed = bleed)
	bm <- page[rows, cols]
	if (hasName(layout, "angle")) {
		bm <- bittermelon::bm_rotate(bm, -layout$angle[index], clockwise = FALSE)
	}
	bm
}

bm_card_rows <- function(page, ..., layout, index, bleed = FALSE) {
	dpi <- get_dpi(page, layout$paper[index], layout$orientation[index])
	if (bleed) {
		height <- layout$height[index] + 2 * layout$bleed[index]
	} else {
		height <- layout$height[index]
	}
	rows <- seq.int(
		from = dpi * (layout$y[index] - 0.5 * height),
		to = dpi * (layout$y[index] + 0.5 * height)
	)
	rows
}

bm_card_cols <- function(page, ..., layout, index, bleed = FALSE) {
	dpi <- get_dpi(page, layout$paper[index], layout$orientation[index])
	if (bleed) {
		width <- layout$width[index] + 2 * layout$bleed[index]
	} else {
		width <- layout$width[index]
	}
	cols <- seq.int(
		from = dpi * (layout$x[index] - 0.5 * width),
		to = dpi * (layout$x[index] + 0.5 * width)
	)
	cols
}

get_dpi <- function(page, paper, orientation) {
	width_in <- paper_width(paper, orientation)
	ncol(page) / width_in
}
