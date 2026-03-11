test_that("`pdf_apply()` errors when `isFALSE(rasterize)` but must rasterize", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(paper = "letter", length = 4L)
	expect_snapshot(
		error = TRUE,
		pdf_apply(f, pages = "even", grid_fn = \() grid::grid.null(), rasterize = FALSE)
	)
})
