test_that("`pdf_create_blank()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 4L)
	expect_equal(qpdf::pdf_length(f), 4L)
})
