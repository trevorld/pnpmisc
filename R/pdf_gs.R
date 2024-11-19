#' Process the pdf file with ghostscript
#'
#' `pdf_gs()` processes the pdf file with ghostscript.
#' This may prevent issues with other pdf processing functions like [pdftools::pdf_pagesize()].
#' @inheritParams pdf_pad_pagesize
#' @param args Arguments to pass to ghostscript.
#'             Automatically adds `-dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sAutoRotatePages=None -sOutputFile={output}`.
#' @examples
#' if (tools::find_gs_cmd()[[1L]] != "") {
#'   f1 <- pdf_create_blank()
#'   f2 <- pdf_gs(f1)
#'
#'   unlink(f1)
#'   unlink(f2)
#' }
#' @export
pdf_gs <- function(input, output = NULL, ..., args = character(0L)) {

    input <- normalizePath(input)
    output <- normalize_output(output, input)

    args <- c("-dBATCH",
              "-dNOPAUSE",
              "-sDEVICE=pdfwrite",
              "-sAutoRotatePages=None",
              paste0("-sOutputFile=", shQuote(output)),
              args,
              shQuote(input))

    stdout <- system2_gs(args)

    invisible(output)
}

# Adapted from `xmpdf:::xmpdf_system2()`
system2_gs <- function(args) {
    cmd <- tools::find_gs_cmd()
    output <- system2(cmd, args, stdout = TRUE)
    if (!is.null(attr(output, "status"))) {
        msg <- c(paste(sQuote("system2()"), "command failed."))
        stop(msg)
    }
    invisible(output)
}
