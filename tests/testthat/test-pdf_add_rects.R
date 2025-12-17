test_that("`pdf_add_lines()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(length = 2L, paper = "letter")
	f2 <- pdf_add_lines(f1, layout = "poker_3x3", dpi = 75)
	expect_equal(qpdf::pdf_length(f2), 2L)
})

test_that("`pdf_add_rects()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(length = 2L, paper = "letter")
	f2 <- pdf_add_rects(f1, layout = "poker_3x3", dpi = 75)
	expect_equal(qpdf::pdf_length(f2), 2L)
})
