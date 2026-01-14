#' Infer the paper size of a PDF document
#'
#' `pdf_paper()` infers the paper size of a PDF document
#' @return A character vector.
#' @inheritParams pdf_pad_paper
#' @examples
#' f <- pdf_create_blank(width = 8.5, height = 11)
#' pdf_paper(f)
#' unlink(f)
#' @export
pdf_paper <- function(input) {
	pagesize <- pdftools::pdf_pagesize(input) # in "bigpts" units
	if (dev.cur() == 1L) {
		grDevices::pdf(NULL)
		on.exit(invisible(dev.off()), add = TRUE)
	}
	height <- convertHeight(unit(pagesize$height, "bigpts"), "in", valueOnly = TRUE)
	width <- convertWidth(unit(pagesize$width, "bigpts"), "in", valueOnly = TRUE)
	short <- pmin(width, height)
	long <- pmax(width, height)
	paper <- rep_len("special", length(height))
	paper <- ifelse(abs(short - 2.25) < 5e-2 & abs(long - 3.5) < 5e-2, "bridge", paper)
	paper <- ifelse(abs(short - 2.5) < 5e-2 & abs(long - 3.5) < 5e-2, "poker", paper)
	paper <- ifelse(abs(short - 8.5) < 5e-2 & abs(long - 14) < 5e-2, "legal", paper)
	paper <- ifelse(abs(short - 8.5) < 5e-2 & abs(long - 11) < 5e-2, "letter", paper)
	paper <- ifelse(abs(short - 11.69) < 5e-2 & abs(long - 16.54) < 5e-2, "a3", paper)
	paper <- ifelse(abs(short - 8.27) < 5e-2 & abs(long - 11.69) < 5e-2, "a4", paper)
	paper <- ifelse(abs(short - 5.83) < 5e-2 & abs(long - 8.27) < 5e-2, "a5", paper)
	paper
}
