#' @importFrom grDevices dev.cur dev.off dev.set pdf
#' @importFrom grid grid.circle grid.newpage grid.rect grid.segments
#' @importFrom grid popViewport pushViewport viewport
#' @importFrom grid convertWidth convertHeight gpar unit
NULL

normalize_output <- function(output, input) {
    if (is.null(output))
        output <- tempfile(fileext = ".pdf")
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    stopifnot(input != output)
    output
}

is_odd <- function(x) (x %% 2L) > 0L
is_even <- function(x) (x %% 2L) == 0L
