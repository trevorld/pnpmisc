test_that("layout functions", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	skip_if_not_installed("bittermelon")

	f1 <- pdf_create_blank(length = 1L, paper = "letter", orientation = "landscape")
	bm <- pdf_render_bm_pixmap(f1)
	layout <- layout_preset("button_shy_rules")
	bml <- bm_split_layout(bm, layout = layout)
	f2 <- normalize_output(NULL)
	pnp_pdf(f2, paper = "letter", orientation = "landscape")
	grid_add_layout(bml, layout)
	invisible(dev.off())

	expect_equal(qpdf::pdf_length(f2), 1L)
})
