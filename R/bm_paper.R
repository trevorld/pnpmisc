#' Infer the paper size of a bitmap
#'
#' `bm_paper()` infers the paper size of a bitmap (e.g. a page rendered by
#' [pdf_render_bm_pixmap()]) from its aspect ratio or, if `dpi` is known, from its size.
#'
#' If `dpi` is `NULL` the paper size is inferred from the aspect ratio of the bitmap alone
#' (which doesn't depend on its orientation or resolution):
#'
#' * The ISO A series paper sizes all share the same aspect ratio so they are all reported as `"a4"`.
#' * The card sizes `"poker"` and `"bridge"` are never inferred from the aspect ratio alone
#'   (pass `dpi` to infer them).  Since a poker card's aspect ratio is within 1% of A4's
#'   a low resolution bitmap of a poker card may even be reported as `"a4"`.
#' * Bitmaps whose aspect ratio doesn't match a supported page size are reported as `"special"`.
#'
#' If `dpi` is given then the size of the bitmap in inches is known
#' and the paper size is inferred the same way as [pdf_paper()].
#' @param x A bitmap object supported by `{bittermelon}`
#'          (e.g. a `bm_pixmap`, `bm_bitmap`, `raster`, `nativeRaster`, or `magick-image` object)
#'          or a [bittermelon::bm_list()] of such objects.
#' @param ... Ignored.
#' @param dpi Dots per inch of `x`.
#'            If `NULL` (default) infer the paper size from the aspect ratio of `x` alone.
#' @return A character vector with one paper size per bitmap
#'         (or per frame of a multi-frame `magick-image`).
#'         If `dpi` is `NULL` one of `"letter"`, `"a4"`, `"legal"`, `"executive"`, or `"special"`
#'         else one of the paper sizes inferred by [pdf_paper()].
#' @seealso [pdf_paper()] to infer the paper size of a pdf.
#' @examples
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   f <- pdf_create_blank(paper = "a4", orientation = "portrait")
#'   page <- pdf_render_bm_pixmap(f, page = 1L, dpi = 50)
#'   bm_paper(page)
#'   bm_paper(page, dpi = 50)
#'   unlink(f)
#' }
#' @export
bm_paper <- function(x, ..., dpi = NULL) {
	check_dots_empty()
	stopifnot(
		requireNamespace("bittermelon", quietly = TRUE),
		"`dpi` must be `NULL` or a positive number" = is.null(dpi) ||
			(is.numeric(dpi) && length(dpi) == 1L && isTRUE(dpi > 0))
	)
	if (bittermelon::is_bm_list(x)) {
		return(unlist(lapply(x, bm_paper, dpi = dpi)) %||% character(0L))
	}
	stopifnot(
		"`x` must be a bitmap object or a bm_list of bitmap objects" = is_supported_bitmap(x)
	)
	width <- bittermelon::bm_widths(x)
	height <- bittermelon::bm_heights(x)
	if (is.null(dpi)) {
		paper_from_ratio(width, height)
	} else {
		paper_from_size(width / dpi, height / dpi)
	}
}

# Classify bitmaps' `width` and `height` (in pixels) by aspect ratio (short
# side divided by long side) alone.  Only the page sizes we can tell apart
# this way are candidates: all ISO A series sizes share the A4 ratio and the
# poker card ratio (0.714) is too close to it (7e-3) to include.
# The nearest page ratio wins if within `tol`: pixel rounding perturbs a page's
# ratio by well under 1e-3 for any plausible dpi while the nearest pair of page
# ratios (A4 and executive) is 17e-3 apart.
# Zero-sized bitmaps (whose ratio is `NaN`) are "special".
paper_from_ratio <- function(width, height) {
	tol <- 5e-3
	sizes <- paper_sizes(c("letter", "a4", "legal", "executive"))
	ratios <- sizes$short / sizes$long
	ratio <- pmin(width, height) / pmax(width, height)
	vapply(ratio, \(r) nearest_paper(abs(ratios - r), sizes$paper, tol), character(1L))
}
