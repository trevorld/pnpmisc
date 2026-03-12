#' Render a pdf page into a bittermelon pixmap object
#'
#' `pdf_render_bm_pixmap()` renders a pdf page into a bittermelon pixmap object.
#' @return A [bittermelon::bm_pixmap()] object.
#' @inheritParams pdf_apply
#' @param page Integer of page to render.
#' @seealso [pdftools::pdf_render_page()], [pdf_render_raster()], [pdf_render_bm_list()]
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   f <- pdf_create_wallet()
#'   bm <- pdf_render_bm_pixmap(f, page = 1L, dpi = 75)
#'   grid::grid.raster(bm)
#'   unlink(f)
#' }
#' @export
pdf_render_bm_pixmap <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))

	bitmap <- pdf_render_array_pdftools(input, page = page, dpi = dpi)
	bittermelon::as_bm_pixmap(bitmap)
}

#' Render all pages of a pdf into bittermelon pixmap objects
#'
#' `pdf_render_bm_list()` renders all pages of a pdf into bittermelon pixmap objects.
#' @return A [bittermelon::bm_list()] of [bittermelon::bm_pixmap()] objects.
#' @inheritParams pdf_render_bm_pixmap
#' @seealso [pdf_render_bm_pixmap()]
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   f <- pdf_create_wallet()
#'   bml <- pdf_render_bm_list(f, dpi = 75)
#'   grid::grid.raster(bml[[1L]])
#'   unlink(f)
#' }
#' @export
pdf_render_bm_list <- function(input, ..., dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))

	pages <- pdf_pages(input, pages = "all")
	bml <- bittermelon::bm_list()
	for (i in pages) {
		bml[[i]] <- pdf_render_bm_pixmap(input, page = i, dpi = dpi)
	}

	bml
}


#' Render a pdf page into a raster object
#'
#' `pdf_render_raster()` renders a pdf page into a raster object.
#' @return If `native = TRUE` returns a `nativeRaster` object else a `raster` object.
#' @inheritParams pdf_apply
#' @param page Integer of page to render.
#' @param native If `TRUE` return a `nativeRaster` object else a `raster` object.
#' @seealso [pdftools::pdf_render_page()], [pdf_render_bm_pixmap()]
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   f <- pdf_create_wallet()
#'   r <- pdf_render_raster(f, page = 1L, dpi = 75)
#'   grid::grid.raster(r)
#'   unlink(f)
#' }
#' @export
pdf_render_raster <- function(input, ..., page = 1L, dpi = 300, native = FALSE) {
	check_dots_empty()
	bitmap <- pdf_render_array_pdftools(input, page = page, dpi = dpi)
	if (native) {
		stopifnot(requireNamespace("bittermelon", quietly = TRUE))
		as.raster(bittermelon::as_bm_pixmap(bitmap), native = TRUE)
	} else {
		as.raster(bitmap)
	}
}

pdf_render_array <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	if (nzchar(find_gs_cmd()) && requireNamespace("png", quietly = TRUE)) {
		pdf_render_array_gs(input, page = page, dpi = dpi)
	} else {
		pdf_render_array_pdftools(input, page = page, dpi = dpi)
	}
}

pdf_render_array_pdftools <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	pdftools::pdf_render_page(input, page = page, dpi = dpi, numeric = TRUE)
}

pdf_render_array_gs <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	stopifnot(requireNamespace("png", quietly = TRUE))
	if (is.raw(input)) {
		tmp_pdf <- tempfile(fileext = ".pdf")
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
