#' Create pdf of blank pages
#'
#' `pdf_create_blank()` creates blank pdf pages.
#'
#' @inheritParams pdf_pad_paper
#' @param length Number of pages to create.
#' @param width,height Paper size in inches if `paper = "special"`.
#' @param orientation Either "portrait" or "landscape".  Ignored if `paper = "special"`.
#' @param grob A grid grob to draw on each page
#'             (e.g. `grid::textGrob("This page intentionally left blank.")`).
#'             Default `NULL` is to draw nothing.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a blank pdf file.
#' @examples
#' f1 <- pdf_create_blank(paper = "a4", orientation = "landscape")
#' f2 <- pdf_create_blank(length = 4L)
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_create_blank <- function(
	output = NULL,
	...,
	length = 1L,
	paper = c("special", "letter", "a4"),
	orientation = c("portrait", "landscape"),
	width = 8.5,
	height = 11,
	bg = "white",
	grob = NULL
) {
	paper <- tolower(paper)
	paper <- match.arg(paper)
	orientation <- match.arg(orientation)
	output <- normalize_output(output)

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
	for (page in seq.int(length)) {
		grid.newpage()
		grid.draw(grob)
	}
	invisible(dev.off())

	invisible(output)
}
