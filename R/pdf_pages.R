#' Get integer vector of subset of pdf pages
#'
#' `pdf_pages()` calculates an integer vector of subset of pdf pages.
#'
#' @inheritParams pdf_apply
#' @examples
#' f <- pdf_create_blank(length = 8L)
#' pdf_pages(f, pages = 1:4)
#' pdf_pages(f, pages = -(1:4))
#' pdf_pages(f, pages = "all")
#' pdf_pages(f, pages = "even")
#' pdf_pages(f, pages = "odd")
#' pdf_pages(f, pages = "interleave")
#' pdf_pages(f, pages = "interleave_first")
#' pdf_pages(f, pages = "interleave_last")
#'
#' unlink(f) # Clean up
#' @return An integer vector.
#' @export
pdf_pages <- function(
	input,
	...,
	pages = c(
		"all",
		"even",
		"odd",
		"2-up saddle stitch",
		"interleave",
		"interleave_first",
		"interleave_last"
	)
) {
	check_dots_empty()
	n <- qpdf::pdf_length(input)
	pages_from_n(n, pages)
}

# Keep in sync with the (literal, so it renders in `\\usage{}`) default of `pdf_pages()`
PAGES_CHOICES <- c(
	"all",
	"even",
	"odd",
	"2-up saddle stitch",
	"interleave",
	"interleave_first",
	"interleave_last"
)

# `pdf_apply()` also calls this directly so it only reads `input` once
pages_from_n <- function(n, pages, call = caller_env()) {
	if (!is.numeric(pages) && !is.character(pages)) {
		abort("`pages` must be numeric or character", call = call)
	}
	if (is.numeric(pages)) {
		pages <- seq.int(n)[pages]
		if (anyNA(pages)) {
			abort(
				c(
					"`pages` must only select pages that exist.",
					i = sprintf("`input` has %d page%s.", n, if (n == 1L) "" else "s")
				),
				class = "pnpmisc_error_pages",
				call = call
			)
		}
	} else {
		pages <- switch(
			arg_match0(pages, PAGES_CHOICES, arg_nm = "pages", error_call = call),
			all = seq.int(n),
			even = which(seq_len(n) %% 2L == 0L),
			odd = which(seq_len(n) %% 2L == 1L),
			`2-up saddle stitch` = pages_2up_saddle_stitch(n),
			interleave = pages_interleave(n),
			interleave_first = pages_interleave_first(n),
			interleave_last = pages_interleave_last(n)
		)
	}
	pages
}

pages_interleave <- function(n) {
	if (n %% 2L != 0L) {
		abort(sprintf("\"interleave\" requires an even number of pages but got %d.", n))
	}
	interleave(seq.int(n %/% 2L), seq.int(n %/% 2L + 1L, n))
}

pages_interleave_first <- function(n) {
	interleave(rep.int(1L, n - 1L), seq_len(n - 1L) + 1L)
}

pages_interleave_last <- function(n) {
	interleave(seq_len(n - 1L), rep.int(n, n - 1L))
}

pages_2up_saddle_stitch <- function(n) {
	if (n %% 4L != 0L) {
		abort(sprintf(
			'"2-up saddle stitch" requires a number of pages divisible by 4 but got %d.',
			n
		))
	}
	x <- seq.int(n)
	pages <- integer(0L)
	while (length(x) > 0L) {
		pages <- c(pages, tail(x, 1L), head(x, 2L), tail(x, 2L)[1L])
		x <- head(tail(x, -2L), -2L)
	}
	pages
}

interleave <- function(x, y) as.integer(rbind(x, y))

# The qpdf wrappers can't do anything useful with an empty selection
check_pages_nonempty <- function(pages, call = caller_env()) {
	if (length(pages) == 0L) {
		abort("`pages` must select at least one page.", class = "pnpmisc_error_pages", call = call)
	}
	invisible(pages)
}
