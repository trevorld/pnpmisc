#' Create printable storage box jacket pdf
#'
#' `pdf_create_jacket()` creates a printable storage box jacket.
#' `pdf_create_4x6_jacket()` is a wrapper targeting a 4x6 photo storage box.
#' `pdf_create_poker_jacket()` is an alternative targeting one or two poker deck storage boxes.
#' `pdf_create_jacket_instructions()` creates a printable sheet of
#' instructions for making such a jacket
#' (these instructions are repeated in the `Details` section below).
#'
#' To make the storage box jacket from the pdf file:
#'
#' `r jacket_instructions_md()`
#'
#' @export
#' @inheritParams pdf_create_blank
#' @param front Fill color/pattern/gradient or grob for front cover section of jacket
#'              Will be drawn in a viewport with a
#'              width of `width` and a height of `height`.
#'              If `NULL` (default) we draw a template.
#'              For `pdf_create_poker_jacket()` may also be a list with one or two of these values (in which case will generate two jackets).
#' @param back Fill color/pattern/gradient or grob for back cover section of jacket
#'              Will be drawn in a viewport with a
#'              width of `width` and a height of `height`.
#'              If `NULL` (default) we draw a template.
#'              For `pdf_create_poker_jacket()` may also be a list with one or two of these values (in which case will generate two jackets).
#' @param spine Fill color/pattern/gradient or grob for spine section of jacket
#'              Will be drawn in a rotated viewport with a
#'              width of `height` and a height of `depth`.
#'              If `NULL` (default) we draw a template.
#'              For `pdf_create_poker_jacket()` may also be a list with one or two of these values (in which case will generate two jackets).
#' @param inner Fill color/pattern/gradient or grob for inner section of jacket.
#'              Will be drawn in a viewport with a width of `2 * width + depth` inches
#'              and a height of `height` inches.
#'              If `NULL` (default) we do not create page for inner section.
#'              For `pdf_create_poker_jacket()` may also be a list with one or two of these values (in which case will generate two jackets).
#' @param width Width of the jacket face as a grid unit.
#'              For a 4x6 photo storage box a good value is `r JACKET_4x6_FRONT_WIDTH` inches and for a poker deck storage box a good value is `r JACKET_POKER_FRONT_WIDTH` inches.
#' @param height Height of the jacket face as a grid unit.
#'              For a 4x6 photo storage box a good value is `r JACKET_4x6_HEIGHT` inches and for a poker deck storage box a good value is `r JACKET_POKER_HEIGHT` inches.
#' @param depth Width of the spine as a grid unit.
#'              For a 4x6 photo storage box a good value is `r JACKET_4x6_SPINE_WIDTH` inches and for a poker deck storage box a good value is `r JACKET_POKER_SPINE_WIDTH` inches.
#' @param bg Background fill for the storage jacket.
#'           Passed to [grid.full()].
#' @examples
#' # Template `front`, `back`, and `spine`
#' if (requireNamespace("piecepackr", quietlyr= TRUE)) {
#'   f1 <- pdf_create_4x6_jacket()
#'   unlink(f1)
#' }
#'
#' # Fill `front`, `back`, and `spine`
#' if (requireNamespace("piecepackr", quietly = TRUE)) {
#'   f2 <- pdf_create_4x6_jacket(front = "red", back = "blue", spine = "purple")
#'   unlink(f2)
#' }
#'
#' # Grob `front`, `back`, and `spine`
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     require("gridpattern", quietly = TRUE)) {
#'   pal <- grDevices::palette()
#'   herringbone <- patternGrob("polygon_tiling", type = "herringbone",
#'                              fill = pal[2], spacing = 0.1)
#'   rhombille <- patternGrob("polygon_tiling", type = "rhombille",
#'                            fill = pal[3], spacing = 0.3)
#'   pythagorean <- patternGrob("polygon_tiling", type = "pythagorean",
#'                              fill = pal[4], spacing = 0.1)
#'   f3 <- pdf_create_4x6_jacket(front = herringbone, back = pythagorean,
#'                           spine = rhombille, bleed = TRUE)
#'   unlink(f3)
#' }
#' @seealso [pdf_add_origami()] to add origami symbols to pre-existing [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home).
#' @export
pdf_create_jacket <- function(
	output = NULL,
	...,
	front = NULL,
	back = NULL,
	spine = NULL,
	inner = NULL,
	paper = getOption("papersize", "letter"),
	orientation = "landscape",
	width = unit(JACKET_4x6_FRONT_WIDTH, "in"),
	height = unit(JACKET_4x6_HEIGHT, "in"),
	depth = unit(JACKET_4x6_SPINE_WIDTH, "in"),
	bg = "transparent"
) {
	chkDots(...)
	output <- normalize_output(output)

	stopifnot(
		requireNamespace("piecepackr", quietly = TRUE),
		is.unit(width),
		is.unit(height),
		is.unit(depth)
	)

	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	pnp_pdf(output, paper = paper, orientation = orientation)

	grid_add_origami(width = width, height = height, depth = depth)

	grid_add_jacket_outer(
		front = front,
		back = back,
		spine = spine,
		width = width,
		height = height,
		depth = depth,
		bg = bg
	)

	if (!is.null(inner)) {
		grid.newpage()
		grid_add_jacket_inner(inner, width, height, depth)
	}

	invisible(dev.off())

	invisible(output)
}

grid_add_jacket_inner <- function(
	inner = NULL,
	width = unit(JACKET_4x6_FRONT_WIDTH, "in"),
	height = unit(JACKET_4x6_HEIGHT, "in"),
	depth = unit(JACKET_4x6_SPINE_WIDTH, "in")
) {
	stopifnot(is.unit(width), is.unit(height), is.unit(depth))

	vp_inner <- viewport(width = 2 * width + depth, height = height)
	pushViewport(vp_inner)
	if (is.null(inner)) {
		grid.null()
	} else if (is_fill(inner)) {
		grid.rect(gp = gpar(col = NA, fill = inner))
	} else {
		grid.draw(inner)
	}
	upViewport()

	invisible(NULL)
}

grid_add_jacket_outer <- function(
	front = NULL,
	back = NULL,
	spine = NULL,
	width = unit(JACKET_4x6_FRONT_WIDTH, "in"),
	height = unit(JACKET_4x6_HEIGHT, "in"),
	depth = unit(JACKET_4x6_SPINE_WIDTH, "in"),
	bg = "transparent"
) {
	stopifnot(is.unit(width), is.unit(height), is.unit(depth))

	# Background
	vp_bg <- viewport(width = 2 * width + depth, height = height)
	grid.full(bg, vp = vp_bg)

	# Front
	vp_front <- viewport(
		x = unit(0.5, "npc") + 0.5 * (width + depth),
		width = width,
		height = height
	)
	pushViewport(vp_front)
	if (is.null(front)) {
		grid.text("Front", gp = gpar(fontsize = 24))
		grid.rect(gp = gpar(col = "black", fill = NA))
	} else if (is_fill(front)) {
		grid.rect(gp = gpar(col = NA, fill = front))
	} else {
		grid.draw(front)
	}
	upViewport()

	# Back
	vp_back <- viewport(
		x = unit(0.5, "npc") - 0.5 * (width + depth),
		width = width,
		height = height
	)
	pushViewport(vp_back)
	if (is.null(back)) {
		grid.text("Back", gp = gpar(fontsize = 24))
		grid.rect(gp = gpar(col = "black", fill = NA))
	} else if (is_fill(back)) {
		grid.rect(gp = gpar(col = NA, fill = back))
	} else {
		grid.draw(back)
	}
	upViewport()

	# Spine
	vp_spine <- viewport(width = height, height = depth, angle = -90)
	pushViewport(vp_spine)
	if (is.null(spine)) {
		grid.text("Spine", gp = gpar(fontsize = 24))
		grid.rect(gp = gpar(col = "black", fill = NA))
	} else if (is_fill(spine)) {
		grid.rect(gp = gpar(col = NA, fill = spine))
	} else {
		grid.draw(spine)
	}
	upViewport()

	piecepackr::grid.cropmark(
		width = 2 * width + depth,
		height = height,
		gp = gpar(col = "black")
	)
	invisible(NULL)
}

#' @rdname pdf_create_jacket
#' @export
pdf_create_4x6_jacket <- function(
	output = NULL,
	...,
	front = NULL,
	back = NULL,
	spine = NULL,
	inner = NULL,
	paper = getOption("papersize", "letter")
) {
	chkDots(...)
	pdf_create_jacket(
		output,
		front = front,
		back = back,
		spine = spine,
		inner = inner,
		paper = paper,
		orientation = "landscape",
		width = unit(JACKET_4x6_FRONT_WIDTH, "in"),
		height = unit(JACKET_4x6_HEIGHT, "in"),
		depth = unit(JACKET_4x6_SPINE_WIDTH, "in")
	)
}

#' @rdname pdf_create_jacket
#' @export
pdf_create_poker_jacket <- function(
	output = NULL,
	...,
	front = NULL,
	back = NULL,
	spine = NULL,
	inner = NULL,
	paper = getOption("papersize", "letter"),
	depth = unit(JACKET_POKER_SPINE_WIDTH, "in"),
	bg = "transparent"
) {
	chkDots(...)
	stopifnot(is.unit(depth))

	# Draw 2 jackets
	if (is_list(front) || is_list(back) || is_list(spine) || is_list(inner)) {
		output <- normalize_output(output)
		stopifnot(requireNamespace("piecepackr", quietly = TRUE))

		if (!is_list(front)) {
			front <- list(front)
		}
		if (!is_list(back)) {
			back <- list(back)
		}
		if (!is_list(spine)) {
			spine <- list(spine)
		}
		if (!is_list(inner)) {
			inner <- list(inner)
		}
		if (length(front) == 1L) {
			front <- front[c(1L, 1L)]
		}
		if (length(back) == 1L) {
			back <- back[c(1L, 1L)]
		}
		if (length(spine) == 1L) {
			spine <- spine[c(1L, 1L)]
		}
		if (length(inner) == 1L) {
			inner <- inner[c(1L, 1L)]
		}

		stopifnot(length(front) == 2L, length(back) == 2L, length(spine) == 2L, length(inner) == 2L)

		current_dev <- dev.cur()
		if (current_dev > 1) {
			on.exit(dev.set(current_dev), add = TRUE)
		}

		width <- unit(JACKET_POKER_FRONT_WIDTH, "in")
		height <- unit(JACKET_POKER_HEIGHT, "in")

		pnp_pdf(output, paper = paper, orientation = "portrait")

		yt <- unit(0.5, "npc") + 1.0 * unit(JACKET_POKER_HEIGHT, "in") - unit(1.218, "in")
		yb <- unit(0.5, "npc") - 1.0 * unit(JACKET_POKER_HEIGHT, "in") + unit(1.218, "in")

		vp <- list(viewport(y = yt), viewport(y = yb))

		for (i in 1:2) {
			pushViewport(vp[[i]])
			grid_add_origami(width = width, height = height, depth = depth)

			grid_add_jacket_outer(
				front = front[[i]],
				back = back[[i]],
				spine = spine[[i]],
				width = width,
				height = height,
				depth = depth,
				bg = bg
			)
			upViewport()
		}

		if (!is.null(inner[[1L]]) || !is.null(inner[[2L]])) {
			grid.newpage()
			for (i in 1:2) {
				pushViewport(vp[[i]])
				grid_add_jacket_inner(inner[[i]], width, height, depth)
				upViewport()
			}
		}

		invisible(dev.off())

		invisible(output)
	} else {
		# Draw 1 jacket
		pdf_create_jacket(
			output,
			front = front,
			back = back,
			spine = spine,
			inner = inner,
			paper = paper,
			width = unit(JACKET_POKER_FRONT_WIDTH, "in"),
			height = unit(JACKET_POKER_HEIGHT, "in"),
			depth = depth,
			bg = bg
		)
	}
}

pdf_create_mock_sbgj <- function(output = NULL) {
	output <- normalize_output(output)
	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	pnp_pdf(output, width = 11, height = 8.5)

	width_fb <- unit(JACKET_4x6_FRONT_WIDTH, "inches")
	width_s <- unit(JACKET_4x6_SPINE_WIDTH, "inches")
	height <- unit(JACKET_4x6_HEIGHT, "inches")
	xc <- unit(0.5, "npc") - unit(0.7, "mm")
	yc <- unit(0.5, "npc") - unit(1.45, "mm")
	grid.rect(
		x = xc,
		y = yc,
		width = 2 * width_fb + width_s,
		height = height,
		gp = gpar(col = NA, fill = "grey")
	)
	grid.rect(x = xc, y = yc, width = width_s, height = height, gp = gpar(col = NA, fill = "black"))

	invisible(dev.off())

	invisible(output)
}
