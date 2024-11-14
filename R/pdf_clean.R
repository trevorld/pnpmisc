#' Copies pdf file while removing temporary pdf files
#'
#' `pdf_clean()` copies `input` pdf file to `output` and removes temporary pdf files with `rm_temp_pdfs(exclude = output)`.
#' @inheritParams pdf_pad
#' @return `output` pdf file name invisibly.
#'         As a side effect copies `input` to `output` and removes temporary pdf files.
#' @examples
#' \dontrun{# May delete user files in `tempdir()`
#' f1 <- tempfile(fileext = ".pdf")
#' pdf(f1)
#' grid::grid.text("")
#' invisible(grDevices::dev.off())
#' 
#' f2 <- pdf_compress(f1)
#' f3 <- pdf_subset(f2, pages = 1L)
#' ls_temp_pdfs()
#' pdf_clean(f3, "output.pdf")
#' ls_temp_pdfs()
#' unlink("output.pdf")
#' }
#' @export
pdf_clean <- function(input, output = NULL, ...) {
    output <- normalize_output(output, input)
    file.copy(to = output, from = input, overwrite = TRUE)
    rm_temp_pdfs(output)
    invisible(output)
}

#' List and remove temporary pdfs
#'
#' Using the functions in this package can lead to several pdf
#' files cluttering your [tempdir()].
#' `ls_temp_pdfs()` lists them while `rm_temp_pdfs()` removes them.
#' @param exclude A character vector of filenames to exclude.
#' @return `ls_temp_pdfs()` returns a character vector.  
#'         `rm_temp_pdfs()` returns `invisible(NULL)`.
#' @rdname rm_temp_pdfs
#' @examples
#' \dontrun{# May delete user pdf files in `tempdir()`
#' f1 <- tempfile(fileext = ".pdf")
#' pdf(f1)
#' grid::grid.text("")
#' invisible(grDevices::dev.off())
#' 
#' f2 <- pdf_compress(f1)
#' f3 <- pdf_subset(f2, pages = 1L)
#' ls_temp_pdfs()
#' rm_temp_pdfs()
#' ls_temp_pdfs()
#' }
#' @export
ls_temp_pdfs <- function(exclude = character()) {
    temp_pdfs <- list.files(tempdir(), pattern = "\\.pdf$", full.names = TRUE)
    setdiff(temp_pdfs, exclude)
}

#' @rdname rm_temp_pdfs
#' @export
rm_temp_pdfs <- function(exclude = character()) {
    temp_pdfs <- ls_temp_pdfs(exclude = exclude)
    unlink(temp_pdfs)
    invisible(NULL)
}
