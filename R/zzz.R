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

SUPPORTED_PAPER <- c("letter", "a4", "poker", "bridge", "legal", "a5", "a3", "special")

paper_width <- function(paper, orientation = "portrait") {
	if (orientation == "portrait") {
		switch(
			paper,
			letter = LETTER_WIDTH,
			legal = 8.5,
			a3 = A4_HEIGHT,
			a4 = A4_WIDTH,
			a5 = A4_HEIGHT / 2,
			bridge = 2.25,
			poker = 2.5,
			paste("Paper", dQuote(paper), "not supported")
		)
	} else {
		# landscape
		paper_height(paper)
	}
}
paper_height <- function(paper, orientation = "portrait") {
	if (orientation == "portrait") {
		switch(
			paper,
			legal = 14,
			letter = LETTER_HEIGHT,
			a3 = 2 * A4_WIDTH,
			a4 = A4_HEIGHT,
			a5 = A4_WIDTH,
			bridge = 3.5,
			poker = 3.5,
			paste("Paper", dQuote(paper), "not supported")
		)
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
	paper = getOption("papersize", "letter"),
	orientation = c("portrait", "landscape"),
	bg = "white"
) {
	paper <- tolower(paper)
	stopifnot(paper %in% SUPPORTED_PAPER)
	if (missing(width) && missing(height)) {
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
	if (requireNamespace("bittermelon") && packageVersion("bittermelon") >= "2.2.0") {
		bittermelon::is_supported_bitmap(x)
	} else {
		inherits(x, c("bm_bitmap", "bm_pixmap", "magick-image", "nativeRaster", "raster"))
	}
}
