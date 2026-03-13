test_that("`pdf_pages()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 8L)
	expect_equal(pdf_pages(f, pages = 1:4), 1:4)
	expect_equal(pdf_pages(f, pages = -(1:4)), 5:8)
	expect_equal(pdf_pages(f, pages = "all"), 1:8)
	expect_equal(pdf_pages(f, pages = "2-up saddle stitch"), c(8L, 1L, 2L, 7L, 6L, 3L, 4L, 5L))
	expect_equal(pdf_pages(f, pages = "even"), seq.int(2, 8, 2))
	expect_equal(pdf_pages(f, pages = "odd"), seq.int(1, 7, 2))
	expect_equal(pdf_pages(f, pages = "interleave"), c(1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L))
	expect_equal(
		pdf_pages(f, pages = "interleave_first"),
		c(1L, 2L, 1L, 3L, 1L, 4L, 1L, 5L, 1L, 6L, 1L, 7L, 1L, 8L)
	)
	expect_equal(
		pdf_pages(f, pages = "interleave_last"),
		c(1L, 8L, 2L, 8L, 3L, 8L, 4L, 8L, 5L, 8L, 6L, 8L, 7L, 8L)
	)

	f3 <- pdf_create_blank(length = 3L)
	expect_snapshot(error = TRUE, pdf_pages(f3, pages = "interleave"))
})
