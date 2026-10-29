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
#' f <- pdf_create_wallet()
#' r <- pdf_render_raster(f, page = 1L, dpi = 75)
#' grid::grid.raster(r)
#' unlink(f)
#' @export
pdf_render_raster <- function(
	input,
	...,
	page = 1L,
	dpi = getOption("pnpmisc.dpi", 300),
	native = FALSE
) {
	check_dots_empty()
	bitmap <- pdf_render_array_pdftools(input, page = page, dpi = dpi)
	if (native) {
		native_raster_from_bitmap(bitmap)
	} else {
		raster_from_bitmap(bitmap)
	}
}

# The pixels of the raw RGBA array (channel x width x height) are already in the
# row-first order that `raster` and `nativeRaster` objects are stored in (see `?as.raster`)
# so we build them directly.  `as.raster(aperm(bitmap))` gives the same `raster` but
# takes twice as long and twice the memory for a 300 dpi page.
raster_from_bitmap <- function(bitmap) {
	channel <- \(k) as.integer(bitmap[k, , ])
	colors <- grDevices::rgb(
		channel(1L),
		channel(2L),
		channel(3L),
		channel(4L),
		maxColorValue = 255L
	)
	structure(colors, dim = dim(bitmap)[3:2], class = "raster")
}

# Each pixel's four RGBA bytes are already the little-endian bytes of the integer
# R uses for colors in a `nativeRaster` so we only need to reinterpret them
native_raster_from_bitmap <- function(bitmap) {
	x <- readBin(bitmap, "integer", n = length(bitmap) %/% 4L, size = 4L, endian = "little")
	structure(x, dim = dim(bitmap)[3:2], class = "nativeRaster")
}

pdf_render_array_pdftools <- function(input, ..., page = 1L, dpi = getOption("pnpmisc.dpi", 300)) {
	check_dots_empty()
	# Strip the "bitmap" class so downstream coercions (ours and bittermelon's) dispatch
	# on the raw array itself: when `{magick}` is loaded its `as.integer.bitmap()` and
	# `as.double.bitmap()` methods intercept them, e.g. scrambling the pixels in
	# bittermelon (<= 2.3.1) (#74).
	# bittermelon (>= 2.4.0) handles the classed array but since R doesn't enforce
	# version floors in `Suggests` we can't rely on users having it.
	unclass(pdftools::pdf_render_page(input, page = page, dpi = dpi))
}
