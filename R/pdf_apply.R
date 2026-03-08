#' Modify selected pages on a pdf
#'
#' `pdf_apply()` renders each page of a pdf as a raster image and
#'  on selected `pages` additionally calls `bm_fn()` to modify those raster images
#'  as well as `grid_fn` overlay custom graphics on top.
#'
#' * The original pdf document will be rasterized.
#'
#' @param input Input pdf filename.
#' @param output Output pdf filename.  `NULL` defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Ignored.
#' @param dpi Dots per inch.  Passed to [pdftools::pdf_render_page()].
#' @param bg `output` pdf background color.
#' @param pages A positive numeric vector of pages to include,
#'              a negative numeric vector of pages to exclude,
#'              or a string:\describe{
#'              \item{all}{Include all the pages.}
#'              \item{even}{Include just the even pages.}
#'              \item{odd}{Include just the odd pages.}
#'              \item{2-up saddle stitch}{The order of the pages to create a saddle-stitch booklet if printing 2-up.}
#'              }
#' @param paper If `NULL` (default) the output pdf will be the same size as the input pdf.
#'              Otherwise the output pdf will be created with this paper size
#'              (see [pdf_pad_paper()] for supported values but most commonly "letter" or "a4").
#' @param scale A numeric scalar to scale the content by.
#'              Values less than 1 shrink the content within the page; values greater than 1 enlarge it.
#'              By default the original content is **not** rescaled.
#'              Note this also adjusts the size of the [grid::viewport()] the `grid_fn` will draw in.
#' @param bm_fn A one-argument function called to edit the selected pages rasterized.  The input will be a "raster" object and the output must be an object coercible to a "raster" object by [grDevices::as.raster()]
#' @param grid_fn A zero-argument function called to draw graphics on selected pages.
#'               Defaults to [grid::grid.null()].
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a pdf with overlaid graphics.
#' @examples
#' f1 <- pdf_create_blank(length = 2L, paper = "letter", bg = "magenta")
#' if (requireNamespace("bittermelon", quietly = TRUE)) {
#'   f2 <- pdf_apply(f1, pages = "even", dpi = 75,
#'                   bm_fn = bittermelon::bm_invert,
#'                   grid_fn = function() {
#'                       grid::grid.text("DRAFT", gp = grid::gpar(col = "red",
#'                                       fontsize = 72, alpha = 0.3))
#'                   })
#'   unlink(f2)
#' }
#' unlink(f1)
#' @export
pdf_apply <- function(
	input,
	output = NULL,
	...,
	pages = "all",
	dpi = 300,
	paper = NULL,
	bg = "white",
	scale = 1,
	bm_fn = identity,
	grid_fn = grid::grid.null
) {
	chkDots(...)
	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}
	pages <- pdf_pages(input, pages = pages)
	output <- normalize_output(output, input)

	df_size_orig <- pdftools::pdf_pagesize(input)
	stopifnot(nrow(df_size_orig) > 0L)

	if (is.null(paper)) {
		width <- unit(df_size_orig$width[1L], "bigpts")
		height <- unit(df_size_orig$height[1L], "bigpts")
		width_in <- convertWidth(width, "inches", valueOnly = TRUE)
		height_in <- convertHeight(height, "inches", valueOnly = TRUE)
		if (current_dev == 1L) {
			invisible(dev.off()) # `convertWidth()` opened device
		}
		pnp_pdf(output, width = width_in, height = height_in, bg = bg)
	} else {
		pnp_pdf(output, paper = paper, orientation = pdf_orientation(input)[1L], bg = bg)
	}
	for (i in seq_len(nrow(df_size_orig))) {
		grid.newpage()

		width <- unit(scale * df_size_orig$width[i], "bigpts")
		height <- unit(scale * df_size_orig$height[i], "bigpts")
		vp <- viewport(width = width, height = height)

		r <- pdf_render_raster(input, page = i, dpi = dpi)
		if (i %in% pages) {
			r <- bm_fn(r)
			grid.raster(r, interpolate = FALSE, vp = vp)
			grid_fn()
		} else {
			grid.raster(r, interpolate = FALSE, vp = vp)
		}
	}
	invisible(dev.off())
	invisible(output)
}
