#' A rectangular grob filled with a color, image, or pattern.
#'
#' `fillGrob()` is a wrapper around [grid::rectGrob()] for a single
#' rectangle grob filled with a color, bitmap, grob, or pattern.
#' In particular grob or bitmap fills will be automatically converted to a [grid::pattern()] fill by `as_fill()`.
#' `grid.fill()` creates a `fillGrob()` and draws it to the screen.
#' @param fill A color string, a [grid::grob()], a [grid::pattern()], or a bitmap like a [bittermelon::bm_pixmap()] or raster object.
#' @param ... Ignored for now.
#' @param width,height If `fill` is a bitmap object then these will be passed to [grid::rasterGrob()].
#' @param vp,name Passed to [grid::rectGrob()].
#' @returns `fillGrob()` and `grid.fill()` return [grid::rectGrob()] object.
#'   As a side effect `grid.fill()` draws to the active graphics device.  `as_fill()` returns a supported grid fill object such as a color string or a `grid::pattern()` object.
#' @examples
#' grob <- fillGrob(grid::radialGradient())
#' grid::grid.draw(grob)
#' @export
fillGrob <- function(
	fill = "transparent",
	...,
	width = NULL,
	height = NULL,
	vp = NULL,
	name = NULL
) {
	chkDots(...)
	fill <- as_fill(fill, width = width, height = height)
	rectGrob(gp = gpar(col = "transparent", fill = fill), vp = vp, name = name)
}

#' @rdname fillGrob
#' @export
grid.fill <- function(...) {
	grob <- fillGrob(...)
	grid.draw(grob)
	invisible(grob)
}

#' @rdname fillGrob
#' @export
as_fill <- function(fill = "transparent", ..., width = NULL, height = NULL) {
	if (inherits(fill, c("bm_pixmap", "magick-image", "raster"))) {
		fill <- as.raster(fill) |>
			rasterGrob(width = width, height = height) |>
			pattern()
	} else if (inherits(fill, "grob")) {
		fill <- pattern(fill)
	}
	fill
}
