test_that("`pdf_rm_crosshairs()`", {
	skip_if_not_installed("bittermelon")
	skip_if_not_installed("piecepackr", "1.14.0-6")
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(length = 2L, orientation = "landscape")
	f2 <- pdf_add_crosshairs(f1, pages = "all", layout = "poker_3x2_bleed", dpi = 75)
	expect_snapshot(
		f3 <- pdf_rm_crosshairs(f2, pages = "odd", layout = "poker_3x2_bleed", dpi = 75)
	)
	expect_equal(qpdf::pdf_length(f2), 2L)
	expect_equal(qpdf::pdf_length(f3), 2L)

	f4 <- pdf_add_cropmarks(f2, pages = "all", layout = "poker_3x2_bleed", dpi = 75)
	expect_equal(qpdf::pdf_length(f4), 2L)
})

test_that("`pdf_rm_crosshairs()` centers a preset name on the input pdf's paper", {
	skip_if_not_installed("bittermelon")
	skip_if_not_installed("piecepackr", "1.14.0-6")
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(paper = "a4", orientation = "landscape")
	f2 <- pdf_add_crosshairs(f1, layout = layout_preset("poker_3x2_bleed", paper = "a4"))
	f3 <- suppressMessages(pdf_rm_crosshairs(f2, layout = "poker_3x2_bleed", dpi = 75))
	f4 <- suppressMessages(
		pdf_rm_crosshairs(f2, layout = layout_preset("poker_3x2_bleed", paper = "a4"), dpi = 75)
	)
	render <- \(f) pdftools::pdf_render_page(f, page = 1L, dpi = 50, numeric = TRUE)
	expect_equal(render(f3), render(f4))
})

test_that("`bm_rm_crosshairs()` centers a preset name on the bitmap's paper", {
	skip_if_not_installed("bittermelon")
	skip_if_not_installed("piecepackr", "1.14.0-6")
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	layout <- layout_preset("poker_3x2_bleed", paper = "a4")
	f1 <- pdf_create_blank(paper = "a4", orientation = "landscape")
	f2 <- pdf_add_crosshairs(f1, layout = layout)
	page <- pdf_render_bm_pixmap(f2, page = 1L, dpi = 50)
	expect_equal(
		bm_rm_crosshairs(page, layout = "poker_3x2_bleed"),
		bm_rm_crosshairs(page, layout = layout)
	)

	special <- pdf_render_bm_pixmap(pdf_create_blank(width = 8.27, height = 11), dpi = 50)
	expect_snapshot(error = TRUE, bm_rm_crosshairs(special, layout = "poker_3x2_bleed"))
})
