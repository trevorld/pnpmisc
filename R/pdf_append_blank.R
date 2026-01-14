#' Append blank pages to a pdf
#'
#' `pdf_append_blank()` appends blank pages to a pdf.
#'
#' @inheritParams pdf_pad_paper
#' @param minimum Final number of pages should be at least this integer.
#' @param multiples_of Final number of pages should be a multiple of this integer.
#' @examples
#' f1 <- pdf_create_blank(length = 1L)
#' f2 <- pdf_append_blank(f1, multiples_of = 4L)
#' qpdf::pdf_length(f2)
#'
#' # Clean up
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_append_blank <- function(input, output = NULL, ..., minimum = 1L, multiples_of = 1L) {
	chkDots(...)
	output <- normalize_output(output, input)
	minimum <- as.integer(minimum)
	multiples_of <- as.integer(multiples_of)

	input_length <- qpdf::pdf_length(input)
	target <- input_length

	if (target < minimum) {
		target <- minimum
	}

	remainder <- target %% multiples_of
	if (remainder != 0L) {
		target <- target + (multiples_of - remainder)
	}

	to_add <- target - input_length
	if (to_add == 0L) {
		file.copy(input, output, overwrite = TRUE)
	} else {
		new <- pdf_create_blank(
			length = to_add,
			width = pdf_width(input, numeric = TRUE)[1L],
			height = pdf_height(input, numeric = TRUE)[1L]
		)
		on.exit(unlink(new), add = TRUE)
		qpdf::pdf_combine(c(input, new), output = output)
	}

	invisible(output)
}
