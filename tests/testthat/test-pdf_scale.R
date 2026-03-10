test_that("`pdf_scale()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	input <- pdf_create_blank(paper = "letter", bg = "blue")
	output <- pdf_scale(input, scale = 0.9, dpi = 75)
	expect_equal(pdf_width(output, numeric = TRUE), 8.5)
	expect_equal(pdf_height(output, numeric = TRUE), 11)
})
