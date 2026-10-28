#' Infer the paper size of a PDF document
#'
#' `pdf_paper()` infers the paper size of a PDF document
#' @return A character vector.
#' @inheritParams pdf_apply
#' @seealso [bm_paper()] to infer the paper size of a bitmap.
#' @examples
#' f <- pdf_create_blank(width = 8.5, height = 11)
#' pdf_paper(f)
#' unlink(f)
#' @export
pdf_paper <- function(input) {
	pagesize <- pdftools::pdf_pagesize(input) # in "bigpts" units where 72 bigpts = 1"
	paper_from_size(pagesize$width / 72, pagesize$height / 72)
}

# Classify page `width` and `height` (in inches) as a supported paper size:
# the nearest paper size wins if both its dimensions are within `tol`.
# PDFs store dimensions as integer bigpts, so the inch round-trip introduces
# quantization error (~0.007 in for A4). 5e-2 is the tightest value that
# passes tests for all supported paper sizes.
paper_from_size <- function(width, height) {
	tol <- 5e-2
	sizes <- paper_sizes()
	short <- pmin(width, height)
	long <- pmax(width, height)
	vapply(
		seq_along(short),
		function(i) {
			d <- pmax(abs(sizes$short - short[i]), abs(sizes$long - long[i]))
			nearest_paper(d, sizes$paper, tol)
		},
		character(1L)
	)
}

# The (portrait) short and long sides in inches of the fixed paper sizes
paper_sizes <- function(papers = setdiff(SUPPORTED_PAPER, "special")) {
	data.frame(
		paper = papers,
		short = vapply(papers, paper_width, numeric(1L), USE.NAMES = FALSE),
		long = vapply(papers, paper_height, numeric(1L), USE.NAMES = FALSE)
	)
}

# The paper whose distance `d` is smallest if that distance is within `tol` else "special"
nearest_paper <- function(d, papers, tol) {
	if (isTRUE(min(d) < tol)) papers[which.min(d)] else "special"
}
