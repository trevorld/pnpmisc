test_that("miscellaneous functions", {
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	skip_if(tools::find_gs_cmd()[[1L]] == "")

	f1 <- pdf_create_blank()
	f2 <- pdf_gs(f1)

	expect_equal(qpdf::pdf_length(f2), 1L)
	expect_equal(pdf_height(f2, numeric = TRUE), 11)
	expect_equal(pdf_width(f2, numeric = TRUE), 8.5)
	expect_equal(pdf_orientation(f2), "portrait")

	expect_equal(pdf_height(f2, units = "bigpts", numeric = TRUE), 11 * 72)
	expect_equal(pdf_width(f2, units = "bigpts", numeric = TRUE), 8.5 * 72)

	expect_true(file.exists(f2))
	f3 <- pdf_clean(f2)
	expect_false(file.exists(f2))
	expect_true(file.exists(f3))

	f4 <- pdf_compress(f3)
	f5 <- pdf_rotate_pages(f4, angle = 90)
	f6 <- pdf_subset(f5, pages = 1L)
	f7 <- pdf_gs(f6)
	expect_equal(qpdf::pdf_length(f7), 1L)
	expect_equal(pdf_height(f7, numeric = TRUE), 8.5)
	expect_equal(pdf_width(f7, numeric = TRUE), 11)
})

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

test_that("`pdf_orientation()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f2 <- pdf_create_blank(width = 11, height = 8.5)
	expect_equal(pdf_orientation(f2), "landscape")
	expect_equal(pdf_paper(f2), "letter")
})

test_that("`pdf_paper()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(paper = "legal", orientation = "landscape")
	expect_equal(pdf_paper(f1), "legal")

	f2 <- pdf_create_blank(paper = "a3", orientation = "portrait")
	expect_equal(pdf_paper(f2), "a3")

	f3 <- pdf_create_blank(paper = "a4", orientation = "landscape")
	expect_equal(pdf_paper(f3), "a4")

	f4 <- pdf_create_blank(paper = "a4", orientation = "portrait")
	expect_equal(pdf_paper(f4), "a4")

	f5 <- pdf_create_blank(paper = "poker", orientation = "landscape")
	expect_equal(pdf_paper(f5), "poker")

	f6 <- pdf_create_blank(paper = "bridge", orientation = "landscape")
	expect_equal(pdf_paper(f6), "bridge")

	f7 <- pdf_create_blank(width = 6, height = 4)
	expect_equal(pdf_paper(f7), "special")
})
