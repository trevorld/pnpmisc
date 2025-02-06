#' Create printable 4x6 photo box jacket pdf
#'
#' `pdf_create_jacket()` creates a printable 4x6 photo box jacket.
#'
#' To make the 4x6 photo jacket from the pdf file:
#'
#' 1. Print it out
#' 2. Use the crop marks to trim off the left and right edge
#' 3. Make the two indicated mountain folds on both sides of the spine
#' 4. Trim off the top and bottom edge
#' 5. Insert into the 4x6 photo box
#'
#' @export
#' @inheritParams pdf_pad_paper
#' @param front Fill color/pattern/gradient or grob for front cover section of jacket
#'              Will be drawn in a viewport with a
#'              width of `r JACKET_FACE_WIDTH` inches
#'              and a height of `r JACKET_HEIGHT` inches.
#'              If `NULL` (default) we draw a template.
#' @param back Fill color/pattern/gradient or grob for back cover section of jacket
#'              Will be drawn in a viewport with a
#'              width of `r JACKET_FACE_WIDTH` inches
#'              and a height of `r JACKET_HEIGHT` inches.
#'              If `NULL` (default) we draw a template.
#' @param spine Fill color/pattern/gradient or grob for spine section of jacket
#'              Will be drawn in a rotated viewport with a
#'              width of `r JACKET_HEIGHT` inches
#'              and a height of `r JACKET_SPINE_WIDTH` inches.
#'              If `NULL` (default) we draw a template.
#' @param inner Fill color/pattern/gradient or grob for inner section of jacket.
#'              Will be drawn in a viewport with a width of `r JACKET_WIDTH` inches
#'              and a height of `r JACKET_HEIGHT` inches.
#'              If `NULL` (default) we do not create page for inner section.
#' @examples
#' # Template `front`, `back`, and `spine`
#' f1 <- pdf_create_jacket()
#' unlink(f1)
#'
#' # Fill `front`, `back`, and `spine`
#' f2 <- pdf_create_jacket(front = "red", back = "blue", spine = "purple")
#' unlink(f2)
#'
#' # Grob `front`, `back`, and `spine`
#' if (require("gridpattern", quietly = TRUE)) {
#'   pal <- grDevices::palette()
#'   herringbone <- patternGrob("polygon_tiling", type = "herringbone",
#'                              fill = pal[2], spacing = 0.1)
#'   rhombille <- patternGrob("polygon_tiling", type = "rhombille",
#'                            fill = pal[3], spacing = 0.3)
#'   pythagorean <- patternGrob("polygon_tiling", type = "pythagorean",
#'                              fill = pal[4], spacing = 0.1)
#'   f3 <- pdf_create_jacket(front = herringbone, back = pythagorean,
#'                           spine = rhombille, bleed = TRUE)
#'   unlink(f3)
#' }
#' @seealso [pdf_add_origami()] to add origami symbols to pre-existing [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home).
#' @export
pdf_create_jacket <- function(output = NULL, ...,
                              front = NULL,
                              back = NULL,
                              spine = NULL,
                              inner = NULL,
                              paper = c("letter", "a4")) {
    paper <- tolower(paper)
    paper <- match.arg(paper)
    output <- normalize_output(output)

    stopifnot(requireNamespace("piecepackr", quietly = TRUE))

    current_dev <- dev.cur()
    if (current_dev > 1) on.exit(dev.set(current_dev), add = TRUE)

    pnp_pdf(output, paper = paper, orientation = "landscape")

    # Front
    vp_front <- viewport(x = unit(0.5, "npc") + 0.5 * unit(JACKET_FACE_WIDTH + JACKET_SPINE_WIDTH, "in"),
                         width = unit(JACKET_FACE_WIDTH, "in"),
                         height = unit(JACKET_HEIGHT, "in"))
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
    vp_back <- viewport(x = unit(0.5, "npc") - 0.5 * unit(JACKET_FACE_WIDTH + JACKET_SPINE_WIDTH, "in"),
                         width = unit(JACKET_FACE_WIDTH, "in"),
                         height = unit(JACKET_HEIGHT, "in"))
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
    vp_spine <- viewport(width = unit(JACKET_HEIGHT, "in"),
                        height = unit(JACKET_SPINE_WIDTH, "in"),
                        angle = -90)
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
        width = unit(2 * JACKET_FACE_WIDTH + JACKET_SPINE_WIDTH, "in"),
        height = unit(JACKET_HEIGHT, "in"),
        gp = gpar(col = "black")
    )
    draw_jacket_origami()

    if (!is.null(inner)) {
        grid.newpage()
        vp_inner <- viewport(width = unit(JACKET_WIDTH, "in"),
                             height = unit(JACKET_HEIGHT, "in"))
        pushViewport(vp_inner)
        if (is_fill(inner)) {
            grid.rect(gp = gpar(col = NA, fill = inner))
        } else {
            grid.draw(inner)
        }
        upViewport()

    }

    invisible(dev.off())

    invisible(output)
}

pdf_create_mock_sbgj <- function(output = NULL) {
    output <- normalize_output(output)
    current_dev <- dev.cur()
    if (current_dev > 1)
        on.exit(dev.set(current_dev), add = TRUE)

    pnp_pdf(output, width = 11, height = 8.5)

    width_fb <- unit(JACKET_FACE_WIDTH, "inches")
    width_s  <- unit(JACKET_SPINE_WIDTH, "inches")
    height   <- unit(JACKET_HEIGHT, "inches")
    xc <- unit(0.5, "npc") - unit(0.7, "mm")
    yc <- unit(0.5, "npc") - unit(1.45, "mm")
    grid.rect(x = xc, y = yc,
        width = 2 * width_fb + width_s, height = height,
        gp = gpar(col = NA, fill = "grey"))
    grid.rect(x = xc, y = yc,
        width = width_s, height = height,
        gp = gpar(col = NA, fill = "black"))

    invisible(dev.off())

    invisible(output)
}
