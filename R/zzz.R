#' @importFrom grDevices dev.cur dev.off dev.set
#' @import grid
NULL

is_even <- function(x) (x %% 2L) == 0L
is_odd <- function(x) (x %% 2L) > 0L
is_fill <- function(x) {
    if (is.grob(x)) return(FALSE)
    is.na(x) || is.character(x) || inherits(x, "GridPattern")
}

normalize_output <- function(output, input = NULL) {
    if (is.null(output))
        output <- tempfile(fileext = ".pdf")
    output <- normalizePath(output, mustWork = FALSE)
    if (!is.null(input)) {
        input <- normalizePath(input, mustWork = TRUE)
        stopifnot(input != output)
    }
    output
}

paper_width <- function(paper, orientation = "portrait") {
    if (orientation == "portrait") {
        switch(paper, letter = LETTER_WIDTH, a4 = A4_WIDTH)
    } else { # landscape
        paper_height(paper)
    }
}
paper_height <- function(paper, orientation = "portrait") {
    if (orientation == "portrait") {
        switch(paper, letter = LETTER_HEIGHT, a4 = A4_HEIGHT)
    } else { # landscape
        paper_width(paper)
    }
}

# Handle both `paper`/`orientation` or `width`/`height`
# Prefer `cairo_pdf()` if available since better pattern fill support (e.g. wallets)
pnp_pdf <- function(output, ...,
                width = 8.5, height = 11,
                paper = c("special", "letter", "a4"),
                orientation = c("portrait", "landscape"),
                bg = "white") {
    paper <- tolower(paper)
    paper <- match.arg(paper)
    if (paper != "special" ) {
        orientation <- match.arg(orientation)
        width <- paper_width(paper, orientation)
        height <- paper_height(paper, orientation)
    }
    if (capabilities("cairo")[[1L]])
        grDevices::cairo_pdf(output, width = width, height = height,
                             bg = bg, onefile = TRUE, ...)
    else
        grDevices::pdf(output, width = width, height = height,
                       bg = bg, onefile = TRUE, ...)
}
