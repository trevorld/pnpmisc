#' Render a pdf page into a bittermelon pixmap object
#'
#' `pdf_render_bm_pixmap()` renders a pdf page into a bittermelon pixmap object.
#' @return A [bittermelon::bm_pixmap()] object.
#' @inheritParams pdf_pad_paper
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
pdf_render_bm_pixmap <- function(input, ..., page = 1L, dpi = 300) {
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))

	bitmap <- pdftools::pdf_render_page(input, page = page, dpi = dpi, numeric = TRUE)
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
pdf_render_bm_list <- function(input, ..., dpi = 300) {
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
#' @inheritParams pdf_pad_paper
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
	bitmap <- pdftools::pdf_render_page(input, page = page, dpi = dpi, numeric = TRUE)
	if (native) {
		stopifnot(requireNamespace("bittermelon", quietly = TRUE))
		grDevices::as.raster(bittermelon::as_bm_pixmap(bitmap), native = TRUE)
	} else {
		grDevices::as.raster(bitmap)
	}
}
