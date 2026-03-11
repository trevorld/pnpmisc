#' Modify selected pages on a pdf
#'
#' `pdf_apply()` renders each page of a pdf as a raster image and
#'  on selected `pages` additionally calls `bm_fn()` to modify those raster images
#'  as well as `grid_fn` overlay custom graphics on top.
#'
#' * The original pdf document may be rasterized depending on the value of
#'   `bm_fn`, `pages`, `paper`, `scale`, and/or `rasterize` as well as
#'   whether ghostscript is available (see [pdf_gs()]).
#'
#' @param input Input pdf filename.
#' @param output Output pdf filename.  `NULL` defaults to `tempfile(fileext = ".pdf")`.
#' @param ... Ignored.
#' @param dpi Dots per inch.
#'   Passed to [pdftools::pdf_render_page()] when we rasterize the original content.
#'   Defaults to `getOption("pnpmisc.dpi", 300)`.
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
#'   1. (`!is.null(paper)` or `scale != 1`) and ghostscript is unavailable (see [pdf_gs()])
#'   2. `any(pages != "all")` and `!identical(grid_fn, grid::grid.null)`
#'   3. `!identical(bm_fn, identity)`
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
	dpi = getOption("pnpmisc.dpi", 300),
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

	must_grid_a_subset <- any(pages != "all") && !identical(grid_fn, grid::grid.null)
	must_resize_wo_gs <- (!is.null(paper) || scale != 1) && !nzchar(tools::find_gs_cmd())
	must_bm <- !identical(bm_fn, identity)
	must_rasterize <- must_grid_a_subset || must_resize_wo_gs || must_bm

	missing_rasterize <- missing(rasterize) && missing(rasterise)
	rasterize <- rasterize %||% must_rasterize

	if (isFALSE(rasterize)) {
		if (must_rasterize) {
			reasons <- must_rasterize_reasons(must_grid_a_subset, must_resize_wo_gs, must_bm)
			abort(
				c(
					"`isFALSE(rasterize)` but the original pdf contents must be rasterized.",
					reasons
				),
				class = "pnpmisc_error_rasterized"
			)
		}

		pdf_apply_vector(input, output, paper = paper, scale = scale, bg = bg, grid_fn = grid_fn)
	} else {
		if (missing_rasterize) {
			reasons <- must_rasterize_reasons(must_grid_a_subset, must_resize_wo_gs, must_bm)
			inform(
				c(
					"The original pdf contents were rasterized.",
					reasons,
					i = 'Suppress this message with `suppressMessages(expr, classes = "pnpmisc_message_rasterized")`.'
				),
				class = "pnpmisc_message_rasterized"
			)
		}
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

# Currently can only use if pages = "all" and no `bm_fn`
pdf_apply_vector <- function(
	input,
	output,
	...,
	paper = NULL,
	scale = 1,
	bg = "transparent",
	grid_fn = grid::grid.null
) {
	resize <- (!is.null(paper) || scale != 1) && nzchar(tools::find_gs_cmd())
	overlay <- !identical(grid_fn, grid::grid.null)

	if (!resize && !overlay) {
		file.copy(input, output, overwrite = TRUE)
		return(invisible(output))
	}
	if (resize && !overlay) {
		pdf_resize_vector(input, output, scale = scale, paper = paper, bg = bg)
		return(invisible(output))
	}

	df_size <- pdftools::pdf_pagesize(input)
	width_in <- df_size$width[1L] / 72
	height_in <- df_size$height[1L] / 72
	stamp <- tempfile(fileext = ".pdf")
	on.exit(unlink(stamp), add = TRUE)
	pnp_pdf(stamp, width = width_in, height = height_in, bg = "transparent")
	grid.newpage()
	grid_fn()
	invisible(dev.off())
	if (resize) {
		overlaid <- tempfile(fileext = ".pdf")
		on.exit(unlink(overlaid), add = TRUE)
		qpdf::pdf_overlay_stamp(input, stamp = stamp, output = overlaid)
		pdf_resize_vector(overlaid, output, scale = scale, paper = paper, bg = bg)
	} else {
		qpdf::pdf_overlay_stamp(input, stamp = stamp, output = output)
	}
	invisible(output)
}

pdf_resize_vector <- function(input, output, scale = 1, paper = NULL, bg = "transparent") {
	if (!is.null(paper)) {
		paper <- tolower(paper)
	}
	df_size <- pdftools::pdf_pagesize(input)
	old_w <- df_size$width[1L] # bigpts
	old_h <- df_size$height[1L] # bigpts

	if (is.null(paper)) {
		new_w <- old_w
		new_h <- old_h
		extra_args <- NULL
	} else {
		orientation <- pdf_orientation(input)[1L]
		new_w <- paper_width(paper, orientation) * 72 # bigpts
		new_h <- paper_height(paper, orientation) * 72 # bigpts
		extra_args <- c(
			"-dFIXEDMEDIA",
			paste0("-dDEVICEWIDTHPOINTS=", new_w),
			paste0("-dDEVICEHEIGHTPOINTS=", new_h)
		)
	}

	tx <- new_w / 2 - scale * old_w / 2
	ty <- new_h / 2 - scale * old_h / 2

	rgba <- grDevices::col2rgb(bg, alpha = TRUE) / 255
	alpha <- rgba[4L]
	bg_ps <- if (alpha == 0) {
		NULL
	} else if (alpha == 1) {
		sprintf("%g %g %g setrgbcolor clippath fill", rgba[1L], rgba[2L], rgba[3L])
	} else {
		sprintf(
			"<< /ca %g >> setgraphicsstate %g %g %g setrgbcolor clippath fill",
			alpha,
			rgba[1L],
			rgba[2L],
			rgba[3L]
		)
	}

	if (is.null(bg_ps) && scale == 1) {
		ps_code <- sprintf("<</PageOffset [%g %g]>> setpagedevice", tx, ty)
	} else {
		transform_ps <- if (scale == 1) {
			sprintf("%g %g translate", tx, ty)
		} else {
			sprintf("%g %g translate %g %g scale", tx, ty, scale, scale)
		}
		inner_ps <- paste(c(bg_ps, transform_ps), collapse = " ")
		ps_code <- sprintf("<</BeginPage {pop %s}>> setpagedevice", inner_ps)
	}
	args <- c(extra_args, "-c", shQuote(ps_code))
	pdf_gs(input, output, args = args)
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
	if (scale != 1 && !identical(grid_fn, grid::grid.null)) {
		old_w <- df_size_orig$width[1L]
		old_h <- df_size_orig$height[1L]
		overlay_grob <- recordGrob(
			grid_fn(),
			list(grid_fn = grid_fn),
			vp = viewport(width = unit(old_w, "bigpts"), height = unit(old_h, "bigpts"))
		)
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
			if (scale != 1 && !identical(grid_fn, grid::grid.null)) {
				grid.define(overlay_grob, name = "pnpmisc_overlay")
				pushViewport(vp)
				grid.use("pnpmisc_overlay", transform = viewportTransform)
				popViewport()
			} else {
				grid_fn()
			}
		} else {
			grid.raster(r, interpolate = FALSE, vp = vp)
		}
	}
	invisible(dev.off())
	invisible(output)
}

must_rasterize_reasons <- function(must_grid_a_subset, must_resize_wo_gs, must_bm) {
	reasons <- character(0L)
	if (must_grid_a_subset) {
		reasons <- c(
			reasons,
			i = '`any(pages != "all")` but `qpdf::pdf_overlay_stamp()` does not support a `pages` argument as of v1.4.1.'
		)
	}
	if (must_resize_wo_gs) {
		reasons <- c(
			reasons,
			i = "`tools::find_gs_cmd())` couldn't find a suitable `ghostscript`."
		)
	}
	if (must_bm) {
		reasons <- c(
			reasons,
			i = 'We needed to rasterize the contents to edit them.`'
		)
	}
	reasons
}
