#' Tell whether pdf is in portrait or landscape mode
#'
#' `pdf_orientation()` tells whether a pdf is in portrait
#' or landscape mode.  A page is "landscape" if it is wider than it is tall
#' and "portrait" otherwise (so square pages are "portrait").
#' The page size is read before any `/Rotate` page attribute is applied
#' so pages a pdf viewer displays rotated by 90 or 270 degrees are reported
#' with the orientation of their unrotated size.
#' Processing the pdf with [pdf_gs()] first bakes any such rotation into the
#' page size so the displayed orientation is reported.
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
