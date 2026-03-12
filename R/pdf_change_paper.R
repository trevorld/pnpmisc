#' Change pdf paper size
#'
#' `pdf_change_paper()` changes the paper size of a pdf, centering the original
#' contents on the new paper size.
#' `pdf_pad_paper()` is an alias semantically intended for padding an undersized pdf document
#' to a normal paper size (e.g. padding a pdf smaller than letter to letter size).
#'
#' By default these function do **not** scale the original contents
#' (but you can pass on a `scale` argument in the `...` to [pdf_apply()]).
#'
#' @inheritParams pnp_pdf
#' @inheritParams pdf_apply
#' @param ... Passed to [pdf_apply()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a pdf with the new paper size.
#' @examples
#' # Some PnP files' size is the intersection of A4/letter page sizes
#' # i.e. shorter than A4 and narrower than letter.
#' # We usually want pad to full A4 or letter page size.
#' input <- tempfile(fileext = ".pdf")
#' grDevices::pdf(input, width = 8.3, height = 11, bg = "blue")
#' grid::grid.text("")
#' invisible(grDevices::dev.off())
#'
#' pdf_width(input)
#' pdf_height(input)
#'
#' output <- pdf_change_paper(input, paper = "letter")
#' pdf_width(output)
#' pdf_height(output)
#' unlink(output)
#'
#' output_a4 <- pdf_pad_paper(input, paper = "a4")
#' pdf_width(output_a4)
#' pdf_height(output_a4)
#' unlink(output_a4)
#'
#' unlink(input)
#' @export
pdf_change_paper <- function(
	input,
	output = NULL,
	...,
	paper = getOption("papersize", "letter")
) {
	pdf_apply(input, output, ..., paper = paper)
}

#' @rdname pdf_change_paper
#' @export
pdf_pad_paper <- pdf_change_paper
