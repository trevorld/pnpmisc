#' Repeat pages of a pdf
#'
#' `pdf_rep()` repeats pages of a pdf similar to [base::rep()].
#'
#' @inheritParams pdf_pad_paper
#' @inheritParams pdf_pages
#' @param times,length.out,each Passed to [base::rep()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a pdf with repeated pages.
#' @seealso [pdf_subset()]
#' @examples
#' f1 <- pdf_create_blank(length = 3L, paper = "letter")
#' f2 <- pdf_rep(f1, times = 2L)
#' f3 <- pdf_rep(f1, pages = 1:2, each = 2L)
#' unlink(f1)
#' unlink(f2)
#' unlink(f3)
#' @export
pdf_rep <- function(
	input,
	output = NULL,
	...,
	pages = "all",
	times = 1L,
	length.out = NA_integer_,
	each = 1L
) {
	chkDots(...)
	pages <- pdf_pages(input, pages = pages)
	pages <- rep(pages, times = times, length.out = length.out, each = each)
	pdf_subset(input, output, pages = pages)
}
