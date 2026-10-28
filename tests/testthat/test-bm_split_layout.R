test_that("`bm_split_layout()` works as expected", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(orientation = "landscape")
	page <- pdf_render_bm_pixmap(f1, page = 1)
	bml <- bm_split_layout(page, layout = "button_shy_rules")
	expect_equal(length(bml), 8L)

	expect_snapshot(error = TRUE, bm_split_layout("not a pixmap", layout = "button_shy_rules"))
})

test_that("`bm_split_layout()` centers a preset name on the page's paper", {
	skip_if_not_installed("bittermelon")
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	page <- pdf_render_bm_pixmap(pdf_create_blank(paper = "a4", orientation = "portrait"), dpi = 50)
	expect_equal(
		bm_split_layout(page, layout = "poker_3x3"),
		bm_split_layout(page, layout = layout_preset("poker_3x3", paper = "a4"))
	)
})
