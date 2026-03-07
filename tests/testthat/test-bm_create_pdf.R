test_that("`bm_create_pdf()`", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	bml <- bittermelon::bm_list()
	bml[[1L]] <- as.raster(matrix("blue", nrow = 4L, ncol = 1L))
	bml[[2L]] <- as.raster(matrix("red", nrow = 1L, ncol = 4L))
	f1 <- bm_create_pdf(bml, paper = "poker")
	expect_equal(qpdf::pdf_length(f1), 2L)

	f2 <- bm_create_pdf(bml, width = 2.5, height = 3.5)
	expect_equal(qpdf::pdf_length(f2), 2L)
	ps <- pdftools::pdf_pagesize(f2)
	expect_equal(ps$width[1L], 2.5 * 72, tolerance = 0.5)
	expect_equal(ps$height[1L], 3.5 * 72, tolerance = 0.5)
})
