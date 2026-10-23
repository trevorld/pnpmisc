raw_from_zip <- function(zipfile, filename) {
	info <- utils::unzip(zipfile, list = TRUE)
	size <- info[which(info$Name == filename), ]$Length
	con <- unz(zipfile, filename, "rb")
	vec_raw <- readBin(con, "raw", size)
	close(con)
	vec_raw
}

#' Extract/render an image from a zipfile into a bittermelon pixmap object
#'
#' `zip_extract_bm_pixmap()` extracts/renders an image from a zipfile into a [bittermelon::bm_pixmap()] object.
#'
#' @param zipfile Path to a zip file
#' @param filename Filename within zip file
#' @param ... If `filename` is a pdf file then passed to [pdf_render_bm_pixmap()]
#' @return A [bittermelon::bm_pixmap()] object.
#' @examples
#' # Typically used with a print-and-play zip archive downloaded from
#' # e.g. <https://boardgamegeek.com/> but here we create our own zip file
#' if (requireNamespace("bittermelon", quietly = TRUE) &&
#'     nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip")))) {
#'   input <- pdf_create_blank(bg = "blue")
#'   zipfile <- tempfile(fileext = ".zip")
#'   utils::zip(zipfile, input, flags = "-qj")
#'
#'   bm <- zip_extract_bm_pixmap(zipfile, basename(input), dpi = 75)
#'   grid::grid.raster(bm)
#'
#'   unlink(c(input, zipfile))
#' }
#' @export
zip_extract_bm_pixmap <- function(zipfile, filename, ...) {
	stopifnot(requireNamespace("bittermelon", quietly = TRUE))
	vec_raw <- raw_from_zip(zipfile, filename)
	ext <- tolower(file_ext(filename))
	switch(
		ext,
		pdf = pnpmisc::pdf_render_bm_pixmap(vec_raw, ...),
		png = {
			stopifnot(requireNamespace("png", quietly = TRUE))
			bittermelon::as_bm_pixmap(png::readPNG(vec_raw))
		},
		jpeg = {
			stopifnot(requireNamespace("jpeg", quietly = TRUE))
			bittermelon::as_bm_pixmap(jpeg::readJPEG(vec_raw))
		},
		jpg = {
			stopifnot(requireNamespace("jpeg", quietly = TRUE))
			bittermelon::as_bm_pixmap(jpeg::readJPEG(vec_raw))
		},
		abort(paste("Don't know how to handle file extension", dQuote(ext)))
	)
}
