# Renders a page with ghostscript's `pngalpha` device which (unlike `{pdftools}`)
# preserves the alpha channel of the page background
pdf_render_array_gs <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	stopifnot(requireNamespace("png", quietly = TRUE))
	if (is.raw(input)) {
		tmp_pdf <- temp_pdf()
		on.exit(unlink(tmp_pdf), add = TRUE)
		writeBin(input, tmp_pdf)
		input <- tmp_pdf
	}
	tmp <- tempfile(fileext = ".png")
	on.exit(unlink(tmp), add = TRUE)
	system2(
		find_gs_cmd(),
		c(
			"-dBATCH",
			"-dNOPAUSE",
			"-sDEVICE=pngalpha",
			paste0("-dFirstPage=", page),
			paste0("-dLastPage=", page),
			paste0("-r", dpi),
			paste0("-sOutputFile=", shQuote(tmp)),
			shQuote(normalizePath(input))
		),
		stdout = TRUE
	)
	png::readPNG(tmp)
}
