#' Infer the paper size of a PDF document
#'
#' `pdf_paper()` infers the paper size of a PDF document
#' @return A character vector.
#' @inheritParams pdf_apply
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
	# PDFs store dimensions as integer bigpts, so the inch round-trip introduces
	# quantization error (~0.007 in for A4). 5e-2 is the tightest value that
	# passes tests for all supported paper sizes.
	tol <- 5e-2
	paper <- ifelse(abs(short - 2.25) < tol & abs(long - 3.5) < tol, "bridge", paper)
	paper <- ifelse(abs(short - 2.5) < tol & abs(long - 3.5) < tol, "poker", paper)
	paper <- ifelse(abs(short - 8.5) < tol & abs(long - 14) < tol, "legal", paper)
	paper <- ifelse(abs(short - 8.5) < tol & abs(long - 11) < tol, "letter", paper)
	paper <- ifelse(abs(short - 11.69) < tol & abs(long - 16.54) < tol, "a3", paper)
	paper <- ifelse(abs(short - 8.27) < tol & abs(long - 11.69) < tol, "a4", paper)
	paper <- ifelse(abs(short - 5.83) < tol & abs(long - 8.27) < tol, "a5", paper)
	paper
}
