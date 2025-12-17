test_that("`pdf_create_blank()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 4L)
	expect_equal(qpdf::pdf_length(f), 4L)
})

test_that("`pdf_append_blank()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 1L)
	expect_equal(qpdf::pdf_length(f), 1L)

	f2 <- pdf_append_blank(f, minimum = 8L, multiples_of = 4L)
	expect_equal(qpdf::pdf_length(f2), 8L)

	f3 <- pdf_append_blank(f, minimum = 1L, multiples_of = 4L)
	expect_equal(qpdf::pdf_length(f3), 4L)

	f4 <- pdf_append_blank(f, minimum = 8L, multiples_of = 3L)
	expect_equal(qpdf::pdf_length(f4), 9L)

	f5 <- pdf_append_blank(f, minimum = 1L, multiples_of = 1L)
	expect_equal(qpdf::pdf_length(f5), 1L)
})

test_that("`bm_create_pdf()`", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	bml <- bittermelon::bm_list()
	bml[[1L]] <- as.raster(matrix("blue", nrow = 4L, ncol = 1L))
	bml[[2L]] <- as.raster(matrix("red", nrow = 1L, ncol = 4L))
	f1 <- bm_create_pdf(bml, paper = "poker")
	expect_equal(qpdf::pdf_length(f1), 2L)
})
