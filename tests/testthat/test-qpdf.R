test_that("`pdf_overlay_stamp()` with `pages`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(paper = "letter", length = 4L)
	stamp <- pdf_create_blank(paper = "letter", bg = "blue")

	local_mocked_bindings(qpdf_overlay_supports_pages = \() FALSE)
	f2 <- pdf_overlay_stamp(f1, stamp = stamp, pages = "all")
	expect_equal(qpdf::pdf_length(f2), 4L)
	expect_snapshot(error = TRUE, pdf_overlay_stamp(f1, stamp = stamp, pages = "odd"))
})

test_that("`pdf_overlay_stamp()` with `pages` when `qpdf` supports it", {
	skip_if_not(qpdf_overlay_supports_pages())
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(paper = "letter", length = 4L)
	stamp <- pdf_create_blank(paper = "letter", bg = "blue")
	f2 <- pdf_overlay_stamp(f1, stamp = stamp, pages = "odd")
	expect_equal(qpdf::pdf_length(f2), 4L)
})
