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

test_that("`pdf_add_rects()` centers a preset name on the input pdf's paper", {
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(paper = "a4")
	f2 <- pdf_add_rects(f1, layout = "poker_3x3")
	f3 <- pdf_add_rects(f1, layout = layout_preset("poker_3x3", paper = "a4"))
	f4 <- pdf_add_rects(f1, layout = layout_preset("poker_3x3", paper = "letter"))
	render <- \(f) pdf_render_array_pdftools(f, page = 1L, dpi = 50)
	expect_equal(render(f2), render(f3))
	expect_gt(sum(abs(render(f2) - render(f4))), 0)

	mixed <- pdf_combine(c(pdf_create_blank(paper = "letter"), f1))
	expect_snapshot(error = TRUE, pdf_add_rects(mixed, layout = "poker_3x3"))
	special <- pdf_create_blank(width = 8.27, height = 11)
	expect_snapshot(error = TRUE, pdf_add_rects(special, layout = "poker_3x3"))
	f5 <- pdf_add_rects(special, pages = "even", layout = "poker_3x3")
	expect_equal(tools::md5sum(special), tools::md5sum(f5), ignore_attr = TRUE)
})
