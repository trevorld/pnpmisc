#' Get integer vector of subset of pdf pages
#'
#' `pdf_pages()` calculates an integer vector of subset of pdf pages.
#'
#' @inheritParams pdf_pad_paper
#' @param pages A positive numeric vector of pages to include,
#'              a negative numeric vector of pages to exclude,
#'              or a string:\describe{
#'              \item{all}{Include all the pages.}
#'              \item{even}{Include just the even pages.}
#'              \item{odd}{Include just the odd pages.}
#'              \item{2-up saddle stitch}{The order of the pages to create a saddle-stitch booklet if printing 2-up.}
#'              }
#' @examples
#' f <- pdf_create_blank(length = 8L)
#' pdf_pages(f, pages = 1:4)
#' pdf_pages(f, pages = -(1:4))
#' pdf_pages(f, pages = "all")
#' pdf_pages(f, pages = "even")
#' pdf_pages(f, pages = "odd")
#'
#' unlink(f) # Clean up
#' @return An integer vector.
#' @export
pdf_pages <- function(input, ..., pages = c("all", "even", "odd", "2-up saddle stitch")) {
	stopifnot(is.numeric(pages) || is.character(pages))
	n <- qpdf::pdf_length(input)
	if (is.numeric(pages)) {
		pages <- seq.int(n)[pages]
	} else {
		pages <- switch(
			match.arg(pages),
			all = seq.int(n),
			even = seq.int(2L, n, by = 2L),
			odd = seq.int(1L, n, by = 2L),
			`2-up saddle stitch` = pages_2up_saddle_stitch(n)
		)
	}
	pages
}

pages_2up_saddle_stitch <- function(n) {
	stopifnot(n > 0L, n %% 4L == 0L)
	x <- seq.int(n)
	pages <- integer(0L)
	while (length(x) > 0L) {
		pages <- c(pages, tail(x, 1L), head(x, 2L), tail(x, 2L)[1L])
		x <- head(tail(x, -2L), -2L)
	}
	pages
}
