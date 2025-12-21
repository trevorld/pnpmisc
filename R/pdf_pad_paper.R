#' Pad pdf file (to a larger paper size)
#'
#' `pdf_pad_paper()` makes a pdf file larger by padding it (i.e. adding space to the outside margins).
#' The original images are **not** rescaled.
#' @param input Input pdf filename.
#' @param output Output pdf filename.  `NULL` defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Ignored.
#' @param bg `output` pdf background color.
#' @param dpi Dots per inch.  Passed to [pdftools::pdf_render_page()].
#' @param paper Paper size.  Either "letter", "a4", or "special".
#' @return `output` pdf file name invisibly.
#'         As a side effect creates padded pdf file.
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
#' output <- pdf_pad_paper(input, dpi = 75)
#' pdf_width(output)
#' pdf_height(output)
#' unlink(output)
#'
#' output_a4 <- pdf_pad_paper(input, dpi = 75, paper = "a4")
#' pdf_width(output_a4)
#' pdf_height(output_a4)
#' unlink(output_a4)
#'
#' unlink(input)
#' @export
pdf_pad_paper <- function(
	input,
	output = NULL,
	...,
	bg = "white",
	dpi = 300,
	paper = c("letter", "a4")
) {
	chkDots(...)
	paper <- tolower(paper)
	paper <- match.arg(paper)
	output <- normalize_output(output, input)

	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	df_size_orig <- pdftools::pdf_pagesize(input)
	stopifnot(nrow(df_size_orig) > 0L)

	orientation <- pdf_orientation(input)[1L]
	pnp_pdf(output, paper = paper, orientation = orientation, bg = bg)
	for (i in seq_len(nrow(df_size_orig))) {
		width <- unit(df_size_orig$width[i], "bigpts")
		height <- unit(df_size_orig$height[i], "bigpts")
		vp <- viewport(width = width, height = height)
		grid.newpage()
		bitmap <- pdftools::pdf_render_page(input, page = i, dpi = dpi, numeric = TRUE)
		pushViewport(vp)
		grid.raster(bitmap, interpolate = FALSE)
		popViewport()
	}
	invisible(dev.off())
	invisible(output)
}
