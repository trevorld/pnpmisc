#' Extract components from a print-and-play layout
#'
#' `bm_split_layout()` extracts print-and-play components from a layout.
#' @inheritParams bm_crop_layout
#' @return A [bittermelon::bm_list()] of [bittermelon::bm_pixmap()] objects.
#' @seealso [bm_crop_layout()] to just crop a single component
#' @examples
#' # Create a mock print-and-play layout page of playing cards
#' if (requireNamespace("bittermelon", quietly = TRUE) &&
#'     requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.17.1" &&
#'     capabilities("cairo")) {
#'   layout <- layout_preset("poker_3x3", paper = "letter")
#'   df <- transform(layout, piece_side = "card_face", suit = 1L, rank = 1:9)
#'   cfg <- piecepackr::game_systems(border = FALSE, systems = "playing_cards")$playing_cards
#'
#'   input <- tempfile(fileext = ".pdf")
#'   grDevices::cairo_pdf(input, width = 8.5, height = 11)
#'   piecepackr::pmap_piece(df, cfg = cfg, default.units = "in")
#'   invisible(grDevices::dev.off())
#'
#'   page <- pdf_render_bm_pixmap(input, page = 1L, dpi = 75)
#'   cards <- bm_split_layout(page, layout = layout)
#'   grid::grid.raster(cards[[1L]])
#'
#'   unlink(input)
#' }
#' @export
bm_split_layout <- function(
	page,
	...,
	layout = "button_shy_cards",
	bleed = FALSE
) {
	check_dots_empty()
	stopifnot(
		requireNamespace("bittermelon", quietly = TRUE),
		"`page` must be a bm_pixmap object" = bittermelon::is_bm_pixmap(page)
	)
	layout <- layout_from_bm(layout, page)
	# Sort by name so components are returned in reading order rather than
	# physical layout order (e.g. zine booklet pages become page_1, page_2, ...).
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
