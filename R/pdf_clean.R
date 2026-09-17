#' Copies pdf file while removing temporary pdf files
#'
#' `pdf_clean()` copies `input` pdf file to `output` and removes temporary pdf files with `rm_temp_pdfs(exclude = output)`.
#' @inheritParams pdf_apply
#' @inheritParams rm_temp_pdfs
#' @examples
#' f1 <- pdf_create_blank()
#' f2 <- pdf_compress(f1)
#' f3 <- pdf_subset(f2, pages = 1L)
#' ls_temp_pdfs()
#' output <- tempfile(fileext = ".pdf")
#' pdf_clean(f3, output)
#' ls_temp_pdfs()
#' unlink(output)
#' @export
pdf_clean <- function(input, output = NULL, ..., fileext = "___pnpmisc.pdf") {
	check_dots_empty()
	output <- normalize_output(output, input)
	file.copy(to = output, from = input, overwrite = TRUE)
	rm_temp_pdfs(output, fileext = fileext)
	invisible(output)
}

#' List and remove temporary pdfs
#'
#' Using the functions in this package can lead to several pdf
#' files cluttering your [tempdir()].
#' `ls_temp_pdfs()` lists them while `rm_temp_pdfs()` removes them.
#' By default only the temporary pdf files created by this package are
#' listed/removed.
#' @param exclude A character vector of filenames to exclude.
#' @param fileext File extension of the temporary pdf files to list/remove.
#'                Defaults to `r r2i_fileext`, the file extension
#'                this package gives the temporary pdf files it creates.
#'                Use a more indiscriminate value such as ".pdf" to also
#'                list/remove pdf files in [tempdir()] created by other means.
#' @return `ls_temp_pdfs()` returns a character vector.
#'         `rm_temp_pdfs()` returns `invisible(NULL)`.
#'         As a side effect `rm_temp_pdfs()` deletes the listed files.
#' @rdname rm_temp_pdfs
#' @examples
#' f1 <- pdf_create_blank()
#' f2 <- pdf_compress(f1)
#' f3 <- pdf_subset(f2, pages = 1L)
#' ls_temp_pdfs()
#' rm_temp_pdfs()
#' ls_temp_pdfs()
#' @export
ls_temp_pdfs <- function(exclude = character(), fileext = "___pnpmisc.pdf") {
	temp_pdfs <- list.files(tempdir(), full.names = TRUE)
	temp_pdfs <- temp_pdfs[endsWith(temp_pdfs, fileext)]
	temp_pdfs <- normalizePath(temp_pdfs, mustWork = TRUE)
	exclude <- normalizePath(exclude, mustWork = TRUE)
	setdiff(temp_pdfs, exclude)
}

#' @rdname rm_temp_pdfs
#' @export
rm_temp_pdfs <- function(exclude = character(), fileext = "___pnpmisc.pdf") {
	temp_pdfs <- ls_temp_pdfs(exclude = exclude, fileext = fileext)
	unlink(temp_pdfs)
	invisible(NULL)
}
