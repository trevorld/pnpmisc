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
