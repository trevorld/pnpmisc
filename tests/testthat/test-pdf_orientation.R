test_that("`pdf_orientation()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f2 <- pdf_create_blank(width = 11, height = 8.5)
	expect_equal(pdf_orientation(f2), "landscape")
	expect_equal(pdf_paper(f2), "letter")

	f3 <- pdf_create_blank(width = 8, height = 8)
	expect_equal(pdf_orientation(f3), "portrait")
	expect_equal(pdf_paper(f3), "special")
})
