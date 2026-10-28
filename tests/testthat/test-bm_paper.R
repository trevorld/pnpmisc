test_that("`bm_paper()`", {
	skip_if_not_installed("bittermelon")
	on.exit(rm_temp_pdfs(), add = TRUE)
	render <- \(...) pdf_render_bm_pixmap(pdf_create_blank(...), dpi = 50)

	letter <- render(paper = "letter", orientation = "portrait")
	a4 <- render(paper = "a4", orientation = "landscape")
	expect_equal(bm_paper(letter), "letter")
	expect_equal(bm_paper(a4), "a4")
	expect_equal(bm_paper(as.raster(letter)), "letter")
	expect_equal(bm_paper(bittermelon::bm_list(letter, a4)), c("letter", "a4"))
	expect_equal(bm_paper(bittermelon::bm_list()), character(0L))

	a5 <- render(paper = "a5", orientation = "portrait")
	expect_equal(bm_paper(a5), "a4")
	expect_equal(bm_paper(a5, dpi = 50), "a5")
	expect_equal(bm_paper(a4, dpi = 50), "a4")
	expect_equal(bm_paper(render(paper = "legal", orientation = "portrait")), "legal")
	expect_equal(bm_paper(render(paper = "executive", orientation = "portrait")), "executive")

	card <- bm_crop_layout(letter, layout = layout_preset("poker_3x3", paper = "letter"))
	expect_equal(bm_paper(card), "special")
	expect_equal(bm_paper(card, dpi = 50), "poker")
	expect_equal(bm_paper(render(width = 8.27, height = 11)), "special")

	expect_equal(bm_paper(as.raster(matrix(character(0L), 0L, 0L))), "special")
	expect_snapshot(error = TRUE, bm_paper("not a bitmap"))
	expect_snapshot(error = TRUE, bm_paper(letter, dpi = c(50, 75)))
})

test_that("`paper_from_ratio()` is vectorized (e.g. for multi-frame magick images)", {
	expect_equal(paper_from_ratio(c(85, 297), c(110, 420)), c("letter", "a4"))
})
