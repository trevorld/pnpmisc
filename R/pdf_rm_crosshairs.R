#' Remove crosshairs
#'
#' `pdf_rm_crosshairs()` removes unwanted crosshairs.
#'
#' * In order to work the PnP layout needs a solid color bleed.
#' * The default layout supports [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP letter size v1).
#' * The original pdf document will be rasterized.
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @inheritParams bm_crop_layout
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
	layout = "poker_3x2_bleed",
	pages = "odd",
	dpi = 300
) {
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))
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

		pixmap <- pdf_render_bm_pixmap(input, page = i, dpi = dpi)
		if (i %in% pages) {
			pixmap <- bm_rm_crosshairs_layout(pixmap, layout)
		}

		width <- unit(df_size_orig$width[i], "bigpts")
		height <- unit(df_size_orig$height[i], "bigpts")
		vp <- viewport(width = width, height = height)
		grid.raster(pixmap, interpolate = FALSE, vp = vp)
	}
	invisible(dev.off())
	invisible(output)
}

bm_rm_crosshairs_layout <- function(pixmap, layout = layout_preset("poker_3x2_bleed")) {
	dpi <- get_dpi(pixmap, layout$paper[1L], layout$orientation[1L])
	for (i in seq_len(nrow(layout))) {
		x <- layout$x[i]
		y <- layout$y[i]
		col <- layout$col[i]
		row <- layout$row[i]
		width <- layout$width[i]
		height <- layout$height[i]
		bleed <- layout$bleed[i]

		rows <- bm_card_rows(pixmap, layout = layout, row = row, col = col)
		cols <- bm_card_cols(pixmap, layout = layout, row = row, col = col)
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
