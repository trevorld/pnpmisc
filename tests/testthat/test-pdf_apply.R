# top-left corner alpha (0 = transparent, 1 = opaque)
pdf_bg_opacity <- function(pdf, dpi = 72L) {
	px <- pnpmisc:::pdf_render_array_gs(pdf, page = 1L, dpi = dpi)
	px[1L, 1L, 4L]
}

test_that("by default just copies the file over", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(paper = "letter", length = 4L)
	f2 <- pdf_apply(f1)
	expect_equal(tools::md5sum(f1), tools::md5sum(f2), ignore_attr = TRUE)
})

test_that("`grid_fn` plus scaling", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pnpmisc:::pdf_create_mock_sbgj()
	f2 <- pdf_add_origami(f1, scale = 0.5)
	expect_equal(qpdf::pdf_length(f2), 1L)

	f3 <- pdf_add_origami(f1, scale = 0.5, rasterize = TRUE, dpi = 75, paper = "a4")
	expect_equal(qpdf::pdf_length(f3), 1L)

	local_mocked_bindings(find_gs_cmd = \() "")
	expect_snapshot(f4 <- pdf_add_origami(f1, scale = 0.5))
	expect_equal(qpdf::pdf_length(f4), 1L)
})

test_that("Backgrounds", {
	skip_if_not(nzchar(find_gs_cmd()))
	skip_if_not_installed("png")
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(width = 5, height = 5, bg = "yellow")
	f2 <- pdf_apply(f1, bg = "transparent", paper = "letter", scale = 1.0)
	f3 <- pdf_apply(f1, bg = "transparent", paper = "letter", scale = 1.2)
	f4 <- pdf_apply(f1, bg = "blue", paper = "letter", scale = 1.2)
	expect_equal(pdf_bg_opacity(f2), 0)
	expect_equal(pdf_bg_opacity(f3), 0)
	expect_equal(pdf_bg_opacity(f4), 1)
	gs_version <- numeric_version(system2(find_gs_cmd(), "--version", stdout = TRUE))
	expect_snapshot(f5 <- pdf_apply(f1, bg = "#00008080", paper = "letter"))
	skip_if(
		gs_version < "9.56",
		"ghostscript 9.56 fixed bug in rendering semi-transparent backgrounds on standard paper sizes"
	)
	expect_equal(pdf_bg_opacity(f5), 128 / 255, tolerance = 0.01)
})

test_that("`pdf_apply()` warns when `bg` is ignored in vector path", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(paper = "letter")
	expect_snapshot(pdf_apply(f, bg = "blue", rasterize = FALSE))
})

test_that("`pdf_apply()` errors when `isFALSE(rasterize)` but must rasterize", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(paper = "letter", length = 4L)
	expect_snapshot(
		error = TRUE,
		pdf_apply(f, pages = "even", grid_fn = \() grid::grid.null(), rasterize = FALSE)
	)
})
