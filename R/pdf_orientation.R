#' Tell whether pdf is in portrait or landscape mode
#'
#' `pdf_orientation()` tells whether a pdf is in portrait
#' or landscape mode.
#' @inheritParams pdf_apply
#' @return A character vector with a length equal to the number of pages in `input`.
#' @examples
#' f1 <- pdf_create_blank(width = 8.5, height = 11)
#' pdf_orientation(f1)
#'
#' f2 <- pdf_create_blank(width = 11, height = 8.5, length = 2L)
#' pdf_orientation(f2)
#'
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_orientation <- function(input, ...) {
	check_dots_empty()
	df <- pdftools::pdf_pagesize(input)
	ifelse(df$width > df$height, "landscape", "portrait")
}
