#' Get integer vector of subset of pdf pages
#'
#' `pdf_pages()` calculates an integer vector of subset of pdf pages.
#'
#' @inheritParams pdf_pad_pagesize
#' @param pages A positive numeric vector of pages to include,
#'              a negative numeric vector of pages to exclude,
#'              or "all", "even", or "odd" to include all pages, just the even pages,
#'              or just the odd pages.
#' @examples
#' f <- pdf_blank(length = 8L)
#' pdf_pages(f, pages = 1:4)
#' pdf_pages(f, pages = -(1:4))
#' pdf_pages(f, pages = "all")
#' pdf_pages(f, pages = "even")
#' pdf_pages(f, pages = "odd")
#'
#' unlink(f) # Clean up
#' @return An integer vector.
#' @export
pdf_pages <- function(input, ..., pages = c("all", "even", "odd")) {
    stopifnot(is.numeric(pages) || is.character(pages))
    n <- qpdf::pdf_length(input)
    if (is.numeric(pages)) {
        pages <- seq.int(n)[pages]
    } else {
        pages <- switch(match.arg(pages),
                        all = seq.int(n),
                        even = seq.int(2L, n, by = 2L),
                        odd = seq.int(1L, n, by = 2L))
    }
    pages
}
