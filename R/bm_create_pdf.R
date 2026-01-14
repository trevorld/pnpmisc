#' Create a pdf from a list of images
#'
#' `bm_create_pdf()` creates a pdf from a list of images (each representing a page).
#'
#' @inheritParams pdf_pad_paper
#' @param width,height Paper size in inches.  Ignored if both are missing (in favor of `paper` and `orientation`).
#' @param orientation Either "portrait" or "landscape".  Ignored if `width` and `height` are not missing.
#' @param pages  A list of images (each representing a page).
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a blank pdf file.
#' @examples
#' f1 <- pdf_create_blank(paper = "a4", orientation = "landscape")
#' f2 <- pdf_create_blank(length = 4L)
#' unlink(f1)
#' unlink(f2)
#' @export
bm_create_pdf <- function(
	pages,
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	orientation = c("portrait", "landscape"),
	width = 8.5,
	height = 11,
	bg = "white"
) {
	chkDots(...)
	orientation <- match.arg(orientation)
	output <- normalize_output(output)

	stopifnot(requireNamespace("bittermelon"))

	if (is_supported_bitmap(pages)) {
		pages <- bittermelon::bm_list(pages)
	}

	pages <- bittermelon::as_bm_list(pages)

	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	if (missing(width) && missing(height) && paper != "special") {
		pnp_pdf(
			output,
			paper = paper,
			orientation = orientation,
			bg = bg
		)
	} else {
		pnp_pdf(
			output,
			width = width,
			height = height,
			bg = bg
		)
	}
	for (i in seq_along(pages)) {
		grid.newpage()
		grid.raster(
			as.raster(pages[[i]]),
			interpolate = FALSE
		)
	}
	invisible(dev.off())

	invisible(output)
}
