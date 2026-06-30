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
#' To convert a document between A4 and letter paper without losing any of its
#' original safe zone, scale by the ratio of the binding dimension
#' (the dimension that shrinks) rather than leaving `scale` at its default of 1:
#' \describe{
#'   \item{A4 to letter}{Height is the binding dimension (letter is shorter than A4),
#'         so use `scale = 11 / (297 / 25.4)` (about 0.94).}
#'   \item{Letter to A4}{Width is the binding dimension (A4 is narrower than letter),
#'         so use `scale = (210 / 25.4) / 8.5` (about 0.97).}
#' }
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
#'
#' # Convert an A4 document to letter, preserving its safe zone
#' input_a4 <- pdf_create_blank(paper = "a4", bg = "blue")
#' output_letter <- pdf_change_paper(
#'   input_a4,
#'   paper = "letter",
#'   scale = 11 / (297 / 25.4)
#' )
#' pdf_width(output_letter)
#' pdf_height(output_letter)
#' unlink(input_a4)
#' unlink(output_letter)
#'
#' # Convert a letter document to A4, preserving its safe zone
#' input_letter <- pdf_create_blank(paper = "letter", bg = "blue")
#' output_a4_safe <- pdf_change_paper(
#'   input_letter,
#'   paper = "a4",
#'   scale = (210 / 25.4) / 8.5
#' )
#' pdf_width(output_a4_safe)
#' pdf_height(output_a4_safe)
#' unlink(input_letter)
#' unlink(output_a4_safe)
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
