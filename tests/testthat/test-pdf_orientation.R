test_that("`pdf_orientation()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f2 <- pdf_create_blank(width = 11, height = 8.5)
	expect_equal(pdf_orientation(f2), "landscape")
	expect_equal(pdf_paper(f2), "letter")
})
