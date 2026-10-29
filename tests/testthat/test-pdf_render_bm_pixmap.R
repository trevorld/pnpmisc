test_that("`pdf_render_bm_pixmap()` and `pdf_render_raster()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(bg = "blue")

	expect_s3_class(pdf_render_raster(f, dpi = 10), "raster")
	expect_s3_class(pdf_render_raster(f, dpi = 10, native = TRUE), "nativeRaster")

	skip_if_not_installed("bittermelon")
	expect_s3_class(pdf_render_bm_pixmap(f, dpi = 10), "bm_pixmap")
	expect_s3_class(pdf_render_bm_list(f, dpi = 10), "bm_list")
})

test_that("`pdf_render_raster()` and `pdf_render_bm_pixmap()` match the numeric render", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_wallet()
	numeric <- pdftools::pdf_render_page(f, page = 1L, dpi = 10, numeric = TRUE)
	expect_equal(pdf_render_raster(f, dpi = 10), as.raster(numeric))

	skip_if_not_installed("bittermelon")
	pm <- bittermelon::as_bm_pixmap(numeric)
	expect_equal(pdf_render_bm_pixmap(f, dpi = 10), pm)

	skip_if_not(rlang::is_installed("colorfast") || rlang::is_installed("farver"))
	expect_equal(pdf_render_raster(f, dpi = 10, native = TRUE), as.raster(pm, native = TRUE))
})
