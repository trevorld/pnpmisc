#' Remove crosshairs
#'
#' `pdf_rm_crosshairs()` removes unwanted crosshairs.
#'
#' * In order to work the PnP layout needs a solid color bleed.
#' * The default layout supports [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP letter size v1).
#' * The original pdf document will be rasterized.
#'
#' @inheritParams bm_crop_layout
#' @inheritParams pdf_apply
#' @param ... Passed to [pdf_apply()].
#' @return `output` pdf file name invisibly.
#'         As a side effect removes from crosshairs from a pdf.
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE) &&
#'     requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-5") {
#'   f1 <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
#'   f2 <- pdf_add_crosshairs(f1, pages = "all",
#'                            layout = "poker_3x2_bleed", dpi = 75)
#'   f3 <- pdf_rm_crosshairs(f2, pages = "odd",
#'                           layout = "poker_3x2_bleed", dpi = 75)
#'   unlink(f1)
#'   unlink(f2)
#'   unlink(f3)
#' }
#' @export
pdf_rm_crosshairs <- function(
	input,
	output = NULL,
	...,
	layout = "poker_3x2_bleed"
) {
	pdf_apply(
		input,
		output,
		...,
		bm_fn = \(r) {
			bm_rm_crosshairs(r, layout = layout)
		}
	)
}

#' Remove crosshairs from a raster object
#'
#' `bm_rm_crosshairs()` removes crosshairs from a raster object.
#'
#' * In order to work the PnP layout needs a solid color bleed.
#' * The default layout supports [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP letter size v1).
#'
#' @param x A raster object coercible to a [bittermelon::bm_pixmap()] object.
#' @inheritParams bm_crop_layout
#' @return A [bittermelon::bm_pixmap()] object.
#' @seealso [pdf_rm_crosshairs()] to remove crosshairs from a pdf.
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE) &&
#'     requireNamespace("piecepackr", quietly = TRUE) &&
#'     utils::packageVersion("piecepackr") >= "1.14.0-5") {
#'   f1 <- pdf_create_blank(length = 1L, width = 11, height = 8.5)
#'   f2 <- pdf_add_crosshairs(f1, pages = "all",
#'                            layout = "poker_3x2_bleed", dpi = 75)
#'   page <- pdf_render_bm_pixmap(f2, page = 1L, dpi = 75)
#'   page2 <- bm_rm_crosshairs(page, layout = "poker_3x2_bleed")
#'   unlink(f1)
#'   unlink(f2)
#' }
#' @export
bm_rm_crosshairs <- function(x, ..., layout = "poker_3x2_bleed") {
	chkDots(...)
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	pixmap <- bittermelon::as_bm_pixmap(x)
	dpi <- get_dpi(pixmap, layout$paper[1L], layout$orientation[1L])
	for (i in seq_len(nrow(layout))) {
		x <- layout$x[i]
		y <- layout$y[i]
		col <- layout$col[i]
		row <- layout$row[i]
		width <- layout$width[i]
		height <- layout$height[i]
		bleed <- layout$bleed[i]

		index <- which(layout$row == row & layout$col == col)

		rows <- bm_card_rows(pixmap, layout = layout, index = index)
		cols <- bm_card_cols(pixmap, layout = layout, index = index)

		color <- pixmap[ceiling(quantile(rows, probs = 0.02, names = FALSE)), floor(median(cols))]
		# Lower-left corner
		pixmap[
			seq.int(
				round(dpi * (y - 0.5 * height - 0.82 * bleed)),
				round(dpi * (y - 0.5 * height + 0.82 * bleed))
			),
			seq.int(
				round(dpi * (x - 0.5 * width - 0.82 * bleed)),
				round(dpi * (x - 0.5 * width + 0.82 * bleed))
			)
		] <- color
		# Upper-left corner
		pixmap[
			seq.int(
				round(dpi * (y + 0.5 * height - 0.82 * bleed)),
				round(dpi * (y + 0.5 * height + 0.82 * bleed))
			),
			seq.int(
				round(dpi * (x - 0.5 * width - 0.82 * bleed)),
				round(dpi * (x - 0.5 * width + 0.82 * bleed))
			)
		] <- color
		# Lower-right corner
		pixmap[
			seq.int(
				round(dpi * (y - 0.5 * height - 0.82 * bleed)),
				round(dpi * (y - 0.5 * height + 0.82 * bleed))
			),
			seq.int(
				round(dpi * (x + 0.5 * width - 0.82 * bleed)),
				round(dpi * (x + 0.5 * width + 0.82 * bleed))
			)
		] <- color
		# Upper-right corner
		pixmap[
			seq.int(
				round(dpi * (y + 0.5 * height - 0.82 * bleed)),
				round(dpi * (y + 0.5 * height + 0.82 * bleed))
			),
			seq.int(
				round(dpi * (x + 0.5 * width - 0.82 * bleed)),
				round(dpi * (x + 0.5 * width + 0.82 * bleed))
			)
		] <- color
	}
	pixmap
}
