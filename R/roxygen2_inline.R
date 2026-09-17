# {roxygen2} inline R code so we don't repeat documentation text
# See <https://roxygen2.r-lib.org/articles/rd-formatting.html#inline-code>

r2i_fileext <- paste0("`", TEMP_PDF_FILEEXT, "`")

r2i_temp_pdf <- paste0("a [tempfile()] with a ", r2i_fileext, " file extension")
