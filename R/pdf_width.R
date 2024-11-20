#' Get dimensions of pdf pages
#'
#' [pdf_width()] and [pdf_height()] get the dimensions of the pdf file pages.
#' @return If `numeric = TRUE` a numeric vector else a [grid::unit()] object.
#' @inheritParams pdf_pad_paper
#' @param units Units to use.  See [grid::unit()].
#' @param numeric If `TRUE` return numeric else a [grid::unit()] object.
#' @examples
#' f <- pdf_create_blank(width = 8.5, height = 11)
#' pdf_width(f)
#' pdf_height(f, units = "cm")
#' pdf_height(f, units = "mm", numeric = TRUE)
#' unlink(f)
#' @export
pdf_width <- function(input, ..., units = "inches", numeric = FALSE) {
    width <- pdftools::pdf_pagesize(input)$width
    if (units == "bigpts" && numeric) {
        return (width)
    } else {
        if (dev.cur() == 1L) on.exit(invisible(dev.off()), add = TRUE)
        convertWidth(unit(width, "bigpts"), units, valueOnly = numeric)
    }
}

#' @rdname pdf_width
#' @export
pdf_height <- function(input, ..., units = "inches", numeric = FALSE) {
    height <- pdftools::pdf_pagesize(input)$height
    if (units == "bigpts" && numeric) {
        return (height)
    } else {
        if (dev.cur() == 1L) on.exit(invisible(dev.off()), add = TRUE)
        convertHeight(unit(height, "bigpts"), units, valueOnly = numeric)
    }
}

#' Tell whether pdf is in portrait or landscape mode
#'
#' `pdf_orientation()` tells whether a pdf is in portrait
#' or landscape mode.
#' @inheritParams pdf_pad_paper
#' @return A character vector with a length equal to the number of pages in `input`.
#' @examples
#' f1 <- pdf_create_blank(width = 8.5, height = 11)
#' pdf_orientation(f1)
#'
#' f2 <- pdf_create_blank(width = 11, height = 8.5, length = 2L)
#' pdf_orientation(f2)
#'
#' unlink(f1)
#' unlink(f2)
#' @export
pdf_orientation <- function(input, ...) {
    df <- pdftools::pdf_pagesize(input)
    ifelse(df$width > df$height, "landscape", "portrait")
}
