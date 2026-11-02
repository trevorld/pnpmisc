test_that("`pdf_create_blank()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)

	f <- pdf_create_blank(length = 4L)
	expect_equal(qpdf::pdf_length(f), 4L)

	expect_snapshot(error = TRUE, pdf_create_blank(paper = "a6"))
})

test_that("`pdf_create_blank()` restores the active graphics device", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	expect_devices_restored(pdf_create_blank())
})
