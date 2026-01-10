#' @importFrom grDevices as.raster dev.cur dev.off dev.set
#' @import grid
#' @importFrom stats median quantile
#' @importFrom utils head tail packageVersion hasName
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x
is_even <- function(x) (x %% 2L) == 0L
is_odd <- function(x) (x %% 2L) > 0L
is_fill <- function(x) {
	if (is.grob(x) || inherits(x, "gList")) {
		return(FALSE)
	}
	is.na(x) || is.character(x) || inherits(x, "GridPattern")
}
is_list <- function(x) {
	is.list(x) && !inherits(x, c("grob", "gList"))
}

normalize_output <- function(output, input = NULL) {
	if (is.null(output)) {
		output <- tempfile(fileext = ".pdf")
	}
	output <- normalizePath(output, mustWork = FALSE)
	if (!is.null(input)) {
		input <- normalizePath(input, mustWork = TRUE)
		stopifnot(input != output)
	}
	output
}

paper_width <- function(paper, orientation = "portrait") {
	if (orientation == "portrait") {
		switch(paper, letter = LETTER_WIDTH, a4 = A4_WIDTH, bridge = 2.25, poker = 2.5)
	} else {
		# landscape
		paper_height(paper)
	}
}
paper_height <- function(paper, orientation = "portrait") {
	if (orientation == "portrait") {
		switch(paper, letter = LETTER_HEIGHT, a4 = A4_HEIGHT, bridge = 3.5, poker = 3.5)
	} else {
		# landscape
		paper_width(paper)
	}
}

# Handle both `paper`/`orientation` or `width`/`height`
# Prefer `cairo_pdf()` if available since better pattern fill support (e.g. wallets)
pnp_pdf <- function(
	output,
	...,
	width = 8.5,
	height = 11,
	paper = c("special", "letter", "a4", "poker", "bridge"),
	orientation = c("portrait", "landscape"),
	bg = "white"
) {
	paper <- tolower(paper)
	paper <- match.arg(paper)
	if (paper != "special") {
		orientation <- match.arg(orientation)
		width <- paper_width(paper, orientation)
		height <- paper_height(paper, orientation)
	}
	if (capabilities("cairo")[[1L]]) {
		grDevices::cairo_pdf(output, width = width, height = height, bg = bg, onefile = TRUE, ...)
	} else {
		grDevices::pdf(output, width = width, height = height, bg = bg, onefile = TRUE, ...)
	}
}

is_supported_bitmap <- function(x) {
	if (requireNamespace("bittermelon") && packageVersion("bittermelon") >= "2.2.1") {
		bittermelon::is_supported_bitmap(x)
	} else {
		inherits(x, c("bm_bitmap", "bm_pixmap", "magick-image", "nativeRaster", "raster"))
	}
}
