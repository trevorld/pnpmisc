#' Create a pdf from a list of images
#'
#' `bm_create_pdf()` creates a pdf from a list of images (each representing a page).
#'
#' @inheritParams pdf_pad_paper
#' @param width,height Paper size in inches if `paper = "special"`.
#' @param orientation Either "portrait" or "landscape".  Ignored if `paper = "special"`.
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
	paper = c("special", "letter", "a4", "poker"),
	orientation = c("portrait", "landscape"),
	width = 8.5,
	height = 11,
	bg = "white"
) {
	paper <- tolower(paper)
	paper <- match.arg(paper)
	orientation <- match.arg(orientation)
	output <- normalize_output(output)

	stopifnot(requireNamespace("bittermelon"))

	if (bittermelon:::is_supported_bitmap(pages)) {
		pages <- bittermelon::bm_list(pages)
	}

	pages <- bittermelon::as_bm_list(pages)

	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	pnp_pdf(
		output,
		paper = paper,
		orientation = orientation,
		width = width,
		height = height,
		bg = bg
	)
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
