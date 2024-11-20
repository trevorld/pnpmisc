#' @importFrom grDevices dev.cur dev.off dev.set
#' @import grid
NULL

is_odd <- function(x) (x %% 2L) > 0L
is_even <- function(x) (x %% 2L) == 0L

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

        pg_width <- switch(paper, letter = LETTER_WIDTH, a4 = A4_WIDTH)
        pg_height <- switch(paper, letter = LETTER_HEIGHT, a4 = A4_HEIGHT)

        if (orientation == "landscape") { # landscape
            width <- pg_height
            height <- pg_width
        } else { # portrait
            width <- pg_width
            height <- pg_height
        }
    }
    if (capabilities("cairo")[[1L]])
        grDevices::cairo_pdf(output, width = width, height = height, bg = bg, ...)
    else
        grDevices::pdf(output, width = width, height = height, bg = bg, ...)
}
