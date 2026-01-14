jacket_instructions_md <- function(orientation = NULL) {
	if (is.null(orientation)) {
		flip_md <- ""
	} else {
		flip_md <- paste0(
			" flipping on the ",
			dQuote(ifelse(orientation == "landscape", "short edge", "long edge"))
		)
	}

	sprintf(
		"1. Print the jacket out (ideally on cardstock).

   * Print %s (100%%).
   * To include the jacket inside (if any) print double-sided%s.

2. Use the crop marks and origami symbols to cut and fold the jacket.

   * Exact procedure depends on your tools and preferred methods.
   * Example procedure #1:

     1. Use the origami mountain fold lines to score
        the fold lines (e.g. using a ruler and a penknife).
     2. Use the crop marks to cut the edges off.
     3. Make two mountain folds using the scored lines along the spine.

   * Example procedure #2:

     1. Use the crop marks to cut away the left and right edge.
     2. Make a *valley* fold by touching the origami dots symbols
        along the top edge together
        and then flatten the paper
        to make the fold and then unfold flat.
        Repeat this procedure with the origami dot symbols along the bottom edge.
     3. Cut away the top and bottom edges
        (you may use the ends of the
        origami mountain fold lines as well as the edges of the jacket
        as guide to where to make these cuts).
     4. Make two *mountain* folds by reversing the (valley) creases.

3. Insert the jacket into the storage box.",
		dQuote("actual size"),
		flip_md
	)
}

#' @rdname pdf_create_jacket
#' @param style A style set such as [marquee::classic_style()] to
#'              be passed to [marquee::marquee_grob()].
#' @export
pdf_create_jacket_instructions <- function(
	output = NULL,
	...,
	paper = getOption("papersize", "letter"),
	orientation = "landscape",
	style = marquee::classic_style()
) {
	chkDots(...)
	output <- normalize_output(output)

	stopifnot(requireNamespace("marquee", quietly = TRUE))

	pnp_pdf(output, paper = paper, orientation = orientation)

	width <- unit(paper_width(paper, orientation), "in")
	height <- unit(paper_height(paper, orientation), "in")

	vp <- viewport(width = width, height = height)
	pushViewport(vp)
	text <- paste0(
		'# Storage Box Jacket Setup\n',
		jacket_instructions_md(orientation = orientation)
	)
	mg <- marquee::marquee_grob(
		text,
		style = style,
		x = unit(2 / 8, "in"),
		width = width - unit(4 / 8, "in"),
		y = unit(1, "npc") - unit(2 / 8, "in")
	)
	grid.draw(mg)
	upViewport()
	invisible(dev.off())
	invisible(output)
}
