test_that("`bm_crop_layout()` error paths", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(orientation = "landscape")
	page <- pdf_render_bm_pixmap(f, page = 1L)

	expect_snapshot(error = TRUE, bm_crop_layout("not a pixmap", layout = "button_shy_cards"))
	expect_snapshot(
		error = TRUE,
		bm_crop_layout(page, layout = "button_shy_cards", row = 99L, col = 1L)
	)
	expect_snapshot(
		error = TRUE,
		bm_crop_layout(page, layout = "button_shy_cards", name = "no_such_name")
	)
})

test_that("`bm_crop_layout()` centers a preset name on the page's paper", {
	skip_if_not_installed("bittermelon")
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	page <- pdf_render_bm_pixmap(pdf_create_blank(paper = "a4", orientation = "portrait"), dpi = 50)
	layout <- layout_preset("poker_3x3", paper = "a4")
	expect_equal(
		bm_crop_layout(page, layout = "poker_3x3", row = 2L, col = 2L),
		bm_crop_layout(page, layout = layout, row = 2L, col = 2L)
	)

	special <- pdf_render_bm_pixmap(pdf_create_blank(width = 8.27, height = 11), dpi = 50)
	expect_snapshot(error = TRUE, bm_crop_layout(special, layout = "poker_3x3"))
})
