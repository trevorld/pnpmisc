test_that("`pdf_render_bm_pixmap()` and `pdf_render_raster()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(bg = "blue")

	r <- pdf_render_raster(f, dpi = 10)
	expect_true(inherits(r, "raster"))

	skip_if_not_installed("bittermelon")
	pm <- pdf_render_bm_pixmap(f, dpi = 10)
	expect_true(inherits(pm, "bm_pixmap"))

	skip_if_not_installed("farver")
	nr <- pdf_render_raster(f, dpi = 10, native = TRUE)
	expect_true(inherits(nr, "nativeRaster"))
})
