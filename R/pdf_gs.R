#' Process the pdf file with ghostscript
#'
#' `pdf_gs()` processes the pdf file with ghostscript.
#' This may prevent issues with other pdf processing functions like [pdftools::pdf_pagesize()].
#' @inheritParams pdf_apply
#' @param args Arguments to pass to ghostscript.
#'             We automatically add `-dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sAutoRotatePages=None -sOutputFile={output}`.
#' @seealso [tools::find_gs_cmd()]
#' @examples
#' if (nzchar(tools::find_gs_cmd())) {
#'   f1 <- pdf_create_blank()
#'   f2 <- pdf_gs(f1)
#'
#'   unlink(f1)
#'   unlink(f2)
#' }
#' @export
pdf_gs <- function(input, output = NULL, ..., args = character(0L)) {
	check_dots_empty()
	stopifnot(nzchar(find_gs_cmd()))
	input <- normalizePath(input)
	output <- normalize_output(output, input)

	args <- c(
		"-dBATCH",
		"-dNOPAUSE",
		"-sDEVICE=pdfwrite",
		"-sAutoRotatePages=None",
		paste0("-sOutputFile=", shQuote(output)),
		args,
		"-f",
		shQuote(input)
	)

	stdout <- system2_cmd(find_gs_cmd(), args)

	invisible(output)
}
