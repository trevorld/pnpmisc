test_that("`pdf_render_raster()`, `pdf_render_bm_pixmap()`, and `pdf_render_bm_list()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_wallet()
	numeric <- pdftools::pdf_render_page(f, page = 1L, dpi = 10, numeric = TRUE)
	expect_equal(pdf_render_raster(f, dpi = 10), as.raster(numeric))
	expect_s3_class(pdf_render_raster(f, dpi = 10, native = TRUE), "nativeRaster")

	skip_if_not_installed("bittermelon")
	pm <- bittermelon::as_bm_pixmap(numeric)
	expect_equal(pdf_render_bm_pixmap(f, dpi = 10), pm)
	expect_s3_class(pdf_render_bm_list(f, dpi = 10), "bm_list")

	skip_if_not(has_namespace("colorfast") || has_namespace("farver"))
	expect_equal(pdf_render_raster(f, dpi = 10, native = TRUE), as.raster(pm, native = TRUE))
})
