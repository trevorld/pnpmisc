#' Extract components from a print-and-play layout
#'
#' `bm_split_layout()` extracts print-and-play components from a layout.
#' @param page A [bittermelon::bm_pixmap()] object representing a print-and-play layout page.
#'             Often the output from [pdf_render_bm_pixmap()].
#' @param ... Ignored for now.
#' @param layout Either a layout preset name in [layout_names()] or a data frame
#'               with layout data (as returned by [layout_grid()]).
#' @param bleed Include the bleed (if available).
#' @return A [bittermelon::bm_list()] of [bittermelon::bm_pixmap()] objects.
#' @seealso [bm_crop_layout()] to just crop a single component
#' @examples
#' \dontrun{# User not expected to have this PDF file
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   input <- "A Nice Cuppa - PNP.pdf"
#'   page <- pdf_render_bm_pixmap(input, page = 4L, dpi = 75)
#'   cards <- bm_split_layout(page, layout = "button_shy_cards")
#'   grid::grid.raster(cards[[1L]])
#' }
#' }
#' @export
bm_split_layout <- function(
	page,
	...,
	layout = "button_shy_cards",
	bleed = FALSE
) {
	chkDots(...)
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	stopifnot(
		requireNamespace("bittermelon", quietly = TRUE),
		bittermelon::is_bm_pixmap(page)
	)
	if (hasName(layout, "name")) {
		layout <- layout[order(layout$name), ]
	}
	bml <- bittermelon::bm_list()
	for (i in seq_len(nrow(layout))) {
		col <- layout$col[i]
		row <- layout$row[i]
		bml[[i]] <- bm_crop_layout(page, layout = layout, col = col, row = row, bleed = bleed)
	}
	if (hasName(layout, "name")) {
		names(bml) <- layout$name
	}

	bml
}
