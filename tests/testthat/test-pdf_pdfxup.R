test_that("`pdfxup_version()`", {
	skip_if_not(nzchar(Sys.which("pdfxup")))
	expect_s3_class(pdfxup_version(), "numeric_version")
})

test_that("`pdf_pdfxup()`", {
	skip_if_not(nzchar(Sys.which("pdfxup")))
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- system.file("doc", "Sweave.pdf", package = "utils")
	skip_if_not(file.exists(f1))

	n <- qpdf::pdf_length(f1)
	f2 <- pdf_pdfxup_booklet(f1, paper = "letter")
	expect_equal(qpdf::pdf_length(f2), ceiling(n / 2L))

	f3 <- pdf_pdfxup_booklet(f1, paper = "letter", pages = 1:4, bb_pages = 1:4)
	expect_equal(qpdf::pdf_length(f3), 2L)
})
