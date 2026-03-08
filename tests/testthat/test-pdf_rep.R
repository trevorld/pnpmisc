test_that("`pdf_rep()`", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	f_white <- pdf_create_blank(length = 1L, paper = "letter", bg = "white")
	f_black <- pdf_create_blank(length = 1L, paper = "letter", bg = "black")
	f <- qpdf::pdf_combine(c(f_white, f_black), output = tempfile(fileext = ".pdf"))

	# times = 2: white, black, white, black
	f2 <- pdf_rep(f, times = 2L)
	expect_equal(qpdf::pdf_length(f2), 4L)
	bml2 <- pdf_render_bm_list(f2, dpi = 10)
	expect_equal(as.character(bml2[[1L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml2[[2L]][1L, 1L]), "#000000FF")
	expect_equal(as.character(bml2[[3L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml2[[4L]][1L, 1L]), "#000000FF")

	# each = 2: white, white, black, black
	f3 <- pdf_rep(f, each = 2L)
	expect_equal(qpdf::pdf_length(f3), 4L)
	bml3 <- pdf_render_bm_list(f3, dpi = 10)
	expect_equal(as.character(bml3[[1L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml3[[2L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml3[[3L]][1L, 1L]), "#000000FF")
	expect_equal(as.character(bml3[[4L]][1L, 1L]), "#000000FF")

	# length.out = 3: white, black, white
	f4 <- pdf_rep(f, times = 2L, length.out = 3L)
	expect_equal(qpdf::pdf_length(f4), 3L)
	bml4 <- pdf_render_bm_list(f4, dpi = 10)
	expect_equal(as.character(bml4[[1L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml4[[2L]][1L, 1L]), "#000000FF")
	expect_equal(as.character(bml4[[3L]][1L, 1L]), "#FFFFFFFF")

	# pages subset: only white page, repeated 3 times
	f5 <- pdf_rep(f, pages = 1L, times = 3L)
	expect_equal(qpdf::pdf_length(f5), 3L)
	bml5 <- pdf_render_bm_list(f5, dpi = 10)
	expect_equal(as.character(bml5[[1L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml5[[2L]][1L, 1L]), "#FFFFFFFF")
	expect_equal(as.character(bml5[[3L]][1L, 1L]), "#FFFFFFFF")
})
