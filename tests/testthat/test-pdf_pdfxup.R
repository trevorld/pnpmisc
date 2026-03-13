test_that("`pdf_pdfxup()`", {
	skip_if_not(nzchar(Sys.which("pdfxup")))
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- system.file("doc", "Sweave.pdf", package = "utils")
	skip_if_not(file.exists(f1))

	n <- qpdf::pdf_length(f1)
	f2 <- pdf_pdfxup_booklet(f1, paper = "letter")
	expect_equal(qpdf::pdf_length(f2), ceiling(n / 2L))
})
