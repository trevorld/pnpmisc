#' @importFrom grDevices dev.cur dev.off dev.set pdf
#' @import grid
NULL

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

is_odd <- function(x) (x %% 2L) > 0L
is_even <- function(x) (x %% 2L) == 0L
