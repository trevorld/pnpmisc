test_that("`pdf_pages()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 8L)
	expect_equal(pdf_pages(f, pages = 1:4), 1:4)
	expect_equal(pdf_pages(f, pages = -(1:4)), 5:8)
	expect_equal(pdf_pages(f, pages = "all"), 1:8)
	expect_equal(pdf_pages(f, pages = "2-up saddle stitch"), c(8L, 1L, 2L, 7L, 6L, 3L, 4L, 5L))
	expect_equal(pdf_pages(f, pages = "even"), seq.int(2, 8, 2))
	expect_equal(pdf_pages(f, pages = "odd"), seq.int(1, 7, 2))
})
