test_that("`bm_split_layout()` works as expected", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)

	f1 <- pdf_create_blank(orientation = "landscape")
	page <- pdf_render_bm_pixmap(f1, page = 1)
	bml <- bm_split_layout(page, layout = "button_shy_rules")
	expect_equal(length(bml), 8L)
})
