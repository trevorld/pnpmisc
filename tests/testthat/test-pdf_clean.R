test_that("`rm_temp_pdfs()` only removes our own temporary pdf files", {
	on.exit(rm_temp_pdfs(fileext = ".pdf"), add = TRUE)

	# The exported defaults are literals so they render in `\usage{}`
	expect_equal(formals(ls_temp_pdfs)$fileext, TEMP_PDF_FILEEXT)
	expect_equal(formals(rm_temp_pdfs)$fileext, TEMP_PDF_FILEEXT)
	expect_equal(formals(pdf_clean)$fileext, TEMP_PDF_FILEEXT)

	ours <- pdf_create_blank()
	theirs <- tempfile(fileext = ".pdf")
	file.copy(ours, theirs)
	theirs <- normalizePath(theirs)

	expect_contains(ls_temp_pdfs(), ours)
	expect_equal(intersect(ls_temp_pdfs(), theirs), character(0L))
	expect_contains(ls_temp_pdfs(fileext = ".pdf"), theirs)
	expect_equal(intersect(ls_temp_pdfs(exclude = theirs, fileext = ".pdf"), theirs), character(0L))

	rm_temp_pdfs()
	expect_equal(ls_temp_pdfs(), character(0L))
	expect_contains(ls_temp_pdfs(fileext = ".pdf"), theirs)

	rm_temp_pdfs(fileext = ".pdf")
	expect_equal(ls_temp_pdfs(fileext = ".pdf"), character(0L))
})

test_that("`pdf_clean()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank()
	f2 <- pdf_compress(f1)
	f3 <- pdf_subset(f2, pages = 1L)
	expect_contains(ls_temp_pdfs(), c(f1, f2, f3))

	output <- tempfile(fileext = ".pdf")
	on.exit(unlink(output), add = TRUE)
	f4 <- pdf_clean(f3, output)
	expect_equal(ls_temp_pdfs(), character(0L))
	expect_equal(qpdf::pdf_length(f4), 1L)
})
