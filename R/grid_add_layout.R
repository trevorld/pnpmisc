#' Draw images to a layout with grid
#'
#' `grid_add_layout()` draws images to a layout in grid.
#'
#' @param images a named list of images with all the names in the `name` column of the `layout` argument.  Currently supports raster objects supported by `bittermelon` and grid grob objects.
#' @inheritParams bm_crop_layout
#' @seealso [piecepackr::pmap_piece()] to draw pieces to a layout represented by a [piecepackr::pp_cfg()] object.
#' @return `NULL` invisibly.
#'         As a side effect draws images to the active graphics device.
#' @export
grid_add_layout <- function(images, layout) {
	if (is.character(layout)) {
		layout <- layout_preset(layout)
	}
	stopifnot(
		is.list(images),
		is.data.frame(layout),
		all(layout$name %in% names(images)),
		requireNamespace("bittermelon", quietly = TRUE)
	)

	for (n in layout$name) {
		i <- which(layout$name == n)

		x <- unit(layout$x[i], "in")
		y <- unit(layout$y[i], "in")
		height <- unit(layout$height[i], "in")
		width <- unit(layout$width[i], "in")
		angle <- layout$angle[i]
		vp <- viewport(x = x, y = y, height = height, width = width, angle = angle)

		image <- images[[n]]
		if (bittermelon:::is_supported_bitmap(image)) {
			raster <- as.raster(image)
			grid.raster(raster, interpolate = FALSE, vp = vp)
		} else if (inherits(image, c("grob", "gList"))) {
			pushViewport(vp)
			grid.draw(image)
			popViewport()
		}
	}
	invisible(NULL)
}
