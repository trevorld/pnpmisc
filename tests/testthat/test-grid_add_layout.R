test_that("layout functions", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	skip_if_not_installed("bittermelon")

	f1 <- pdf_create_blank(length = 1L, paper = "letter", orientation = "landscape")
	bm <- pdf_render_bm_pixmap(f1)
	layout <- layout_preset("button_shy_rules")
	bml <- bm_split_layout(bm, layout = layout)
	f2 <- normalize_output(NULL)
	pnp_pdf(f2, paper = "letter", orientation = "landscape")
	grid_add_layout(bml, layout = layout)
	invisible(dev.off())

	expect_equal(qpdf::pdf_length(f2), 1L)
})

test_that("`grid_add_layout()` errors on unsupported image type", {
	skip_if_not_installed("bittermelon")
	layout <- data.frame(name = "a", x = 1, y = 1, width = 1, height = 1, angle = 0)
	expect_snapshot(error = TRUE, grid_add_layout(list(a = "not an image"), layout = layout))
})
