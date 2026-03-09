#' Modify selected pages on a pdf
#'
#' `pdf_apply()` renders each page of a pdf as a raster image and
#'  on selected `pages` additionally calls `bm_fn()` to modify those raster images
#'  as well as `grid_fn` overlay custom graphics on top.
#'
#' * The original pdf document may be rasterized depending on the value of
#'   `rasterize`, `bm_fn`, `pages`, and/or `paper`.
#'
#' @param input Input pdf filename.
#' @param output Output pdf filename.  `NULL` defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Ignored.
#' @param dpi Dots per inch.
#'   Passed to [pdftools::pdf_render_page()] when we rasterize the original content.
#' @param bg `output` pdf background color.
#' @param pages A positive numeric vector of pages to include,
#'              a negative numeric vector of pages to exclude,
#'              or a string:\describe{
#'              \item{all}{Include all the pages.}
#'              \item{even}{Include just the even pages.}
#'              \item{odd}{Include just the odd pages.}
#'              }
#'              The functions [pdf_pages()] and [pdf_subset()] also support the string:\describe{
#'              \item{2-up saddle stitch}{The order of the pages to create a saddle-stitch booklet if printing 2-up.}
#'              }
#' @param rasterize,rasterise If `TRUE` rasterize the original content using [pdftools::pdf_render_page()].
#'   If `FALSE` don't rasterize the original content and throw an error if a requested feature can not yet be implemented without rasterization.
#'   If `NULL` only rasterize if a requested feature requires it.
#'   Currently requires rasterization in the following cases:
#'
#'   1. `!is.null(paper)` or `scale != 1`
#'   2. `any(pages != "all")`
#'   3. `!missing(bm_fn)`
#'
#'   `rasterise` is an alias for `rasterize`.
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
	rasterize = rasterise,
	dpi = 300,
	paper = NULL,
	bg = "white",
	scale = 1,
	bm_fn = identity,
	grid_fn = grid::grid.null,
	rasterise = NULL
) {
	chkDots(...)
	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}
	output <- normalize_output(output, input)
	rasterize <- rasterize %||%
		(any(pages != "all") ||
			!is.null(paper) ||
			scale != 1 ||
			!missing(bm_fn))
	if (isFALSE(rasterize)) {
		if (any(pages != "all")) {
			stop(r"(We can't yet combine `isFALSE(rasterize)` and `any(pages != "all")`)")
		}
		if (scale != 1) {
			stop(r"(We can't yet combine `isFALSE(rasterize)` and `scale != 1`)")
		}
		if (!is.null(paper)) {
			stop(r"(We can't yet combine `isFALSE(rasterize)` and `!is.null(paper)`)")
		}
		if (!is.null(paper)) {
			stop(r"(We can't yet combine `isFALSE(rasterize)` and `!is.null(paper)`)")
		}

		pdf_apply_vector(input, output, grid_fn = grid_fn)
	} else {
		#### If `rasterize` was missing then emit a message?
		pages <- pdf_pages(input, pages = pages)
		pdf_apply_raster(
			input,
			output,
			pages = pages,
			dpi = dpi,
			paper = paper,
			bg = bg,
			scale = scale,
			bm_fn = bm_fn,
			grid_fn = grid_fn
		)
	}
}

# Currently can only use if pages = "all", `scale = 1`, and no `bm_fn`
pdf_apply_vector <- function(input, output, ..., grid_fn = grid::grid.null) {
	df_size <- pdftools::pdf_pagesize(input)
	width_in <- df_size$width[1L] / 72
	height_in <- df_size$height[1L] / 72
	stamp <- tempfile(fileext = ".pdf")
	on.exit(unlink(stamp), add = TRUE)
	pnp_pdf(stamp, width = width_in, height = height_in, bg = "transparent")
	grid.newpage()
	grid_fn()
	invisible(dev.off())
	qpdf::pdf_overlay_stamp(input, stamp = stamp, output = output)
	return(invisible(output))
}

pdf_apply_raster <- function(
	input,
	output,
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
	df_size_orig <- pdftools::pdf_pagesize(input)
	stopifnot(nrow(df_size_orig) > 0L)

	if (is.null(paper)) {
		# size is in "bigpts" units where 72pts = 1"
		width_in <- df_size_orig$width[1L] / 72
		height_in <- df_size_orig$height[1L] / 72
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
