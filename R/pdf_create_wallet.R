#' Create print-and-play card wallet pdf
#'
#' `pdf_create_wallet()` creates print-and-play card origami wallets pdfs.
#'
#' To make the wallets from the pdf files:
#'
#' 1. Print out two-sided flipping on the **long** edge
#' 2. Using the crop marks on the first page (the page showing the "spine") 
#'    trim the edges with a cutting tool
#'    (the four dots should now be at the corner of each page).
#' 3. Make the four corner valley folds each labeled "Folds 1"
#' 4. Make the two valley folds each labeled "Folds 2"
#' 5. Flip over the paper and make the two valley folds each labeled "Folds 3"
#' 6. Fold in half (valley fold)
#'
#' @inheritParams pdf_pad_paper
#' @param front Fill color/pattern/gradient or grob for front cover section of wallet.
#'              Will be drawn in a rotated masked viewport with a 
#'              width of `2 + 2 * bleed` inches
#'              and a height of `8 + 2 * bleed` inches.
#'              If `NULL` (default) we draw a template.
#' @param back Fill color/pattern/gradient or grob for back cover section of wallet.
#'             Will be drawn in a rotated masked viewport with a 
#'             width of `2 * 2 * bleed` inches
#'             and a height of `8 + 2 * bleed` inches.
#'             If `NULL` (default) we draw a template.
#' @param spine Fill color/pattern/gradient or grob for spine section of wallet.
#'              Will be drawn in a masked viewport with a
#'              width of `8 + 2 * bleed` inches
#'              and a height of `6 + 2 * bleed` inches.
#'              If `NULL` (default) we draw a template.
#' @param bleed Bleed zone size to assume:
#'              \itemize{
#'                 \item{If `bleed` is a [grid::unit()] simply use it}
#'                 \item{If `bleed` is numeric then convert via `grid::unit(bleed, "in")`}
#'                 \item{If `bleed` is `TRUE` assume 1/8 inch bleed zone size}
#'                 \item{If `bleed` is `FALSE` assume 0 inch bleed zone size}
#'              }
#' @return `output` pdf file name invisibly.
#'         As a side effect creates a print-and-play card wallet pdf file.
#' @examples
#' # Template `front`, `back`, and `spine`
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     getRversion() >= "4.1") {
#'   f1 <- pdf_create_wallet()
#'   unlink(f1)
#' }
#'
#' # Fill `front`, `back`, and `spine`
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     getRversion() >= "4.1") {
#' f2 <- pdf_create_wallet(front = "red", back = "blue", spine = "purple")
#' unlink(f2)
#' }
#' 
#' # Grob `front`, `back`, and `spine` with bleed
#' if (requireNamespace("piecepackr", quietly = TRUE) &&
#'     getRversion() >= "4.1" &&
#'     require("gridpattern", quietly = TRUE)) {
#'   pal <- grDevices::palette()
#'   herringbone <- patternGrob("polygon_tiling", type = "herringbone", 
#'                              fill = pal[2], spacing = 0.1)
#'   rhombille <- patternGrob("polygon_tiling", type = "rhombille",
#'                            fill = pal[3], spacing = 0.2)
#'   pythagorean <- patternGrob("polygon_tiling", type = "pythagorean",
#'                              fill = pal[4], spacing = 0.1)
#'   f3 <- pdf_create_wallet(front = herringbone, back = rhombille,
#'                           spine = pythagorean, bleed = TRUE)
#'   unlink(f3)
#' }
#' @export
pdf_create_wallet <- function(output = NULL, ...,
                              front = NULL,
                              back = NULL,
                              spine = NULL,
                              bleed = unit(0, "in"),
                              paper = c("letter", "a4")) {
    paper <- tolower(paper)
    paper <- match.arg(paper)
    output <- normalize_output(output)

    stopifnot(requireNamespace("piecepackr", quietly = TRUE))
    stopifnot(getRversion() >= "4.1.0") # e.g. alpha masks

    current_dev <- dev.cur()
    if (current_dev > 1) on.exit(dev.set(current_dev), add = TRUE)

    pnp_pdf(output, paper = paper)

    # Put after `pnp_pdf()` since `convertWidth()` needs graphics device
    if (isTRUE(bleed)) bleed <- 0.125
    if (isFALSE(bleed)) bleed <- 0
    if (grid::is.unit(bleed))
        bleed <- grid::convertWidth(bleed, "in", valueOnly = TRUE)

    draw_wallet_spine(b = bleed, spine = spine)
    grid.newpage()
    draw_wallet_cover(b = bleed, front = front, back = back)
    invisible(dev.off())

    invisible(output)
}

draw_wallet_cover <- function(b = 0, front = NA, back = NA) {
    vp_wallet <- viewport(width = unit(8, "in"), height = unit(10, "in"))
    pushViewport(vp_wallet)
    gp_dashed <- gpar(col = "black", lwd = 2, lty = "dashed")
    gp_solid <- gpar(col = "black", lwd = 2, lty = "solid", fill = "black")
    gp_arrow <- arrow(angle = 30, length = unit(0.125, "in"),
                            ends = "last", type = "closed")
    gp_text <- gpar(col = "black")
    grid.segments(x0 = unit(2.00, "in"), x1 = unit(2.00, "in"),
                  y0 = unit(2.25, "in"), y1 = unit(7.75, "in"),
                  gp = gp_dashed)
    grid.segments(x0 = unit(6.00, "in"), x1 = unit(6.00, "in"),
                  y0 = unit(2.25, "in"), y1 = unit(7.75, "in"),
                  gp = gp_dashed)
    grid.circle(x = unit(c(0, 4, 8), "in"),
                y = unit(5, "in"),
                r = unit(1, "mm"), gp = gpar(col = NA, fill = "black"))
    grid.segments(x0 = unit(0.125, "in"), x1 = unit(3.875, "in"),
                  y0 = unit(5.000, "in"), y1 = unit(5.000, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.segments(x0 = unit(7.875, "in"), x1 = unit(4.125, "in"),
                  y0 = unit(5.000, "in"), y1 = unit(5.000, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.text("Folds 3", x = unit(c(2.5, 5.5), "in"), y = unit(5.25, "in"), gp = gp_text)

    # Front cover
    mask_front <- as.mask(polygonGrob(
                          y = unit(c(0, 0, 8 + 2 * b, 8 + 2 * b, 6 + 2 * b, 2), "in"),
                          x = unit(c(2, 2 + 2 * b, 2 + 2 * b, 2, 0, 0), "in"),
                          gp = gpar(col = NA, fill = "white")))
    vp_front <- viewport(y = unit(9, "in"), 
                         width = unit(2 + 2 * b, "in"),
                         height = unit(8 + 2 * b, "in"),
                         mask = mask_front,
                         angle = -90)
    pushViewport(vp_front)
    if (is.null(front)) {
        draw_mock_front_cover(b)
    } else if (is_fill(front)) {
        grid.rect(gp = gpar(col = NA, fill = front))
    } else {
        grid.draw(front)
    }
    popViewport()

    # Back cover
    mask_back <- as.mask(polygonGrob(
                          y = unit(c(0, 0, 2, 6 + 2 * b, 8 + 2*b, 8 + 2 * b), "in"),
                          x = unit(c(0, 0 + 2*b, 2 + 2*b, 2 + 2*b, 0 + 2*b, 0), "in"),
                          gp = gpar(col = NA, fill = "white")))
    vp_back <- viewport(y = unit(1, "in"), 
                        width = unit(2 + 2 * b, "in"),
                        height = unit(8 + 2 * b, "in"),
                        mask = mask_back,
                        angle = -90)
    pushViewport(vp_back)
    if (is.null(back)) {
        draw_mock_back_cover(b)
    } else if (is_fill(back)) {
        grid.rect(gp = gpar(col = NA, fill = back))
    } else {
        grid.draw(back)
    }
    popViewport()

    popViewport()
    invisible(NULL)
}

draw_wallet_spine <- function(b = unit(0, "in"), spine = NULL) {
    vp_wallet <- viewport(width = unit(8, "in"), height = unit(10, "in"))
    pushViewport(vp_wallet)

    piecepackr::grid.cropmark(width = unit(8, "in"), height = unit(10, "in"), gp = gpar(col = "black"))

    # "3" folds
    gp_dashed <- gpar(col = "black", lwd = 2, lty = "dashed")
    gp_solid <- gpar(col = "black", lwd = 2, lty = "solid", fill = "black")
    gp_arrow <- arrow(angle = 30, length = unit(0.125, "in"),
                            ends = "last", type = "closed")
    gp_text <- gpar(col = "black")
    grid.segments(x0 = unit(0.250, "in"), x1 = unit(1.875, "in"),
                  y0 = unit(1.750, "in"), y1 = unit(0.125, "in"),
                  gp = gp_dashed)
    grid.segments(x0 = unit(7.750, "in"), x1 = unit(6.125, "in"),
                  y0 = unit(1.750, "in"), y1 = unit(0.125, "in"),
                  gp = gp_dashed)
    grid.segments(x0 = unit(0.250, "in"), x1 = unit(1.875, "in"),
                  y0 = unit(8.250, "in"), y1 = unit(9.875, "in"),
                  gp = gp_dashed)
    grid.segments(x0 = unit(7.750, "in"), x1 = unit(6.125, "in"),
                  y0 = unit(8.250, "in"), y1 = unit(9.875, "in"),
                  gp = gp_dashed)

    grid.circle(x = unit(rep(c(0, 2, 6, 8), each = 2L), "in"),
                y = unit(c(0, 10, 2, 8, 2, 8, 0, 10), "in"),
                r = unit(1, "mm"), gp = gpar(col = NA, fill = "black"))
    grid.segments(x0 = unit(0.125, "in"), x1 = unit(1.875, "in"),
                  y0 = unit(0.125, "in"), y1 = unit(1.875, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.segments(x0 = unit(0.125, "in"), x1 = unit(1.875, "in"),
                  y0 = unit(9.875, "in"), y1 = unit(8.125, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.segments(x0 = unit(7.875, "in"), x1 = unit(6.125, "in"),
                  y0 = unit(9.875, "in"), y1 = unit(8.125, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.segments(x0 = unit(7.875, "in"), x1 = unit(6.125, "in"),
                  y0 = unit(0.125, "in"), y1 = unit(1.875, "in"),
                  gp = gp_solid, arrow = gp_arrow)

    grid.text("Folds 1", x = unit(c(1, 1, 7, 7), "in"), y = unit(c(0.25, 9.75, 0.25, 9.75), "in"),
                    gp = gp_text)

    grid.segments(x0 = unit(2.25, "in"), x1 = unit(5.75, "in"),
                  y0 = unit(2.00, "in"), y1 = unit(2.00, "in"),
                  gp = gp_dashed)
    grid.segments(x0 = unit(2.25, "in"), x1 = unit(5.75, "in"),
                  y0 = unit(8.00, "in"), y1 = unit(8.00, "in"),
                  gp = gp_dashed)

    grid.segments(x0 = unit(4.00, "in"), x1 = unit(4.00, "in"),
                  y0 = unit(0.25, "in"), y1 = unit(3.75, "in"),
                  gp = gp_solid, arrow = gp_arrow)
    grid.segments(x0 = unit(4.00, "in"), x1 = unit(4.00, "in"),
                  y0 = unit(9.75, "in"), y1 = unit(6.25, "in"),
                  gp = gp_solid, arrow = gp_arrow)

    grid.text("Folds 2", x = unit(c(4.5, 3.5), "in"), y = unit(c(1.75, 7.75), "in"),
             gp = gp_text)

    mask_spine <- as.mask(polygonGrob(
        x = unit(c(0, 0, 0+2*b, 2+2*b, 6, 8, 8+2*b, 8+2*b, 8, 6, 2+2*b, 0+2*b), "in"),
        y = unit(c(6+2*b, 0, 0, 2, 2, 0, 0, 6+2*b, 6+2*b, 4+2*b, 4+2*b, 6+2*b), "in"),
        gp = gpar(col = NA, fill = "white")))
    vp_spine <- viewport(width = unit(8 + 2 * b, "in"),
                         height = unit(6 + 2 * b, "in"),
                         mask = mask_spine)
    pushViewport(vp_spine)
    if (is.null(spine)) {
        draw_mock_spine(b)
    } else if (is_fill(spine)) {
        grid.rect(gp = gpar(col = NA, fill = spine))
    } else {
        grid.draw(spine)
    }
    popViewport()

    popViewport()
    invisible(NULL)
}

draw_mock_front_cover <- function(b) {
    grid.rect(gp = gpar(col = NA, fill = "grey95"))

    pushViewport(viewport(width = unit(2, "in"), height = unit(8, "in")))
    gp_dotdash <- gpar(col = "black", lwd = 2, lty = "dotdash")
    gp_text_note <- gpar(fontsize = 24, col = "black")
    gp_text_note_small <- gpar(fontsize = 12, col = "black")
    grid.text("Front\nCover", gp = gp_text_note)
    grid.text("Inside\nLower\nLeft", rot = 180, x = unit(1.5, "in"), y = unit(1.5, "in"),
              gp = gp_text_note_small)
    grid.text("Inside\nUpper\nLeft", rot = 180, x = unit(1.5, "in"), y = unit(6.5, "in"),
              gp = gp_text_note_small)
    grid.segments(y0 = unit(2, "in"), y1 = unit(2, "in"), x0 = unit(0.25, "in"), x1 = unit(1.75, "in"),
                  gp = gp_dotdash)
    grid.segments(y0 = unit(6, "in"), y1 = unit(6, "in"), x0 = unit(0.25, "in"), x1 = unit(1.75, "in"),
                  gp = gp_dotdash)
    grid::grid.polygon(y = unit(c(0, 8, 6, 2), "in"),
                       x = unit(c(2, 2, 0, 0), "in"),
                       gp = gpar(col = "black", fill = NA))
    popViewport()
    invisible(NULL)
}

draw_mock_back_cover <- function(b) {
    grid.rect(gp = gpar(col = NA, fill = "grey95"))
    pushViewport(viewport(width = unit(2, "in"), height = unit(8, "in")))
    gp_dotdash <- gpar(col = "black", lwd = 2, lty = "dotdash")
    gp_text_note <- gpar(fontsize = 24, col = "black")
    gp_text_note_small <- gpar(fontsize = 12, col = "black")
    grid.text("Back\nCover", x = unit(1, "in"), gp = gp_text_note, rot = 0)
    grid.text("Inside\nLower\nRight", rot = 180, y = unit(1.5, "in"), x = unit(0.5, "in"),
              gp = gp_text_note_small)
    grid.text("Inside\nUpper\nRight", rot = 180, y = unit(6.5, "in"), x = unit(0.5, "in"),
              gp = gp_text_note_small)
    grid.segments(y0 = unit(2, "in"), y1 = unit(2, "in"), x0 = unit(0.25, "in"), x1 = unit(1.75, "in"),
                  gp = gp_dotdash)
    grid.segments(y0 = unit(6, "in"), y1 = unit(6, "in"), x0 = unit(0.25, "in"), x1 = unit(1.75, "in"),
                  gp = gp_dotdash)
    grid::grid.polygon(y = unit(c(0, 2, 6, 8), "in"),
                       x = unit(c(0, 2, 2, 0), "in"),
                       gp = gpar(col = "black", fill = NA))
    popViewport()
    invisible(NULL)
}

draw_mock_spine <- function(b) {
    grid.rect(gp = gpar(col = NA, fill = "grey95"))

    pushViewport(viewport(width = unit(8, "in"), height = unit(6, "in")))
    gp_text_note <- gpar(fontsize = 24, col = "black")
    gp_dotdash <- gpar(col = "black", lwd = 2, lty = "dotdash")
    grid.text("Inside Bottom", x = unit(7, "in"), rot = -90, gp = gp_text_note)
    grid.text("Inside Top", x = unit(1, "in"), rot = -90, gp = gp_text_note)
    grid.text("Spine", gp = gp_text_note)
    grid.segments(x0 = unit(2, "in"), x1 = unit(2, "in"), 
                  y0 = unit(2.25, "in"), y1 = unit(3.75, "in"),
                  gp = gp_dotdash)
    grid.segments(x0 = unit(6, "in"), x1 = unit(6, "in"), 
                  y0 = unit(2.25, "in"), y1 = unit(3.75, "in"),
                  gp = gp_dotdash)
    grid::grid.polygon(x = unit(c(0, 0, 2, 6, 8, 8, 6, 2), "in"),
                       y = unit(c(6, 0, 2, 2, 0, 6, 4, 4), "in"),
                       gp = gpar(col = "black", fill = NA))
    popViewport()
    invisible(NULL)
}
