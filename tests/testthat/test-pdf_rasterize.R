test_that("`pdf_rasterize()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(length = 2L, paper = "letter")
	f2 <- pdf_rasterize(f1, dpi = 75)
	expect_equal(qpdf::pdf_length(f2), 2L)
})
