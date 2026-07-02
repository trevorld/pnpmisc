test_that("`pdf_change_paper()`", {
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)
	on.exit(rm_temp_pdfs(), add = TRUE)

	# portrait
	input <- pdf_create_blank(width = 8.3, height = 11, bg = "blue")
	expect_equal(pdf_width(input, numeric = TRUE), 8.3, tolerance = 0.01)
	expect_equal(pdf_height(input, numeric = TRUE), 11)

	output <- pdf_change_paper(input)
	expect_equal(pdf_width(output, numeric = TRUE), 8.5)
	expect_equal(pdf_height(output, numeric = TRUE), 11)

	output_a4 <- pdf_change_paper(input, paper = "a4")
	expect_equal(pdf_width(output_a4, numeric = TRUE), 8.3, tolerance = 0.01)
	expect_equal(pdf_height(output_a4, numeric = TRUE), 11.7, tolerance = 0.01)

	# landscape
	input <- pdf_create_blank(width = 11, height = 8.3, bg = "blue")
	expect_equal(pdf_width(input, numeric = TRUE), 11)
	expect_equal(pdf_height(input, numeric = TRUE), 8.3, tolerance = 0.01)

	output <- pdf_change_paper(input)
	expect_equal(pdf_width(output, numeric = TRUE), 11)
	expect_equal(pdf_height(output, numeric = TRUE), 8.5)

	output_a4 <- pdf_change_paper(input, paper = "A4")
	expect_equal(pdf_width(output_a4, numeric = TRUE), 11.7, tolerance = 0.01)
	expect_equal(pdf_height(output_a4, numeric = TRUE), 8.3, tolerance = 0.01)
})

test_that("`pdf_change_paper()` resizes width when input has an explicit `/CropBox`", {
	skip_if_not(nzchar(find_gs_cmd()))
	gs_version <- numeric_version(system2(find_gs_cmd(), "--version", stdout = TRUE))
	skip_if(
		gs_version < "10.1",
		"ghostscript 10.01.0 added -dModifiesPageSize to stop pdfwrite from propagating the input's /CropBox"
	)
	on.exit(rm_temp_pdfs(), add = TRUE)

	input <- pdf_create_blank(width = 8.3, height = 11, bg = "blue")
	cropped <- tempfile(fileext = ".pdf")
	pdf_gs(input, cropped, args = c("-c", shQuote("[/CropBox [0 0 597.6 792] /PAGE pdfmark")))
	expect_equal(pdf_width(cropped, numeric = TRUE), 8.3, tolerance = 0.01)

	output <- pdf_change_paper(cropped, paper = "letter")
	expect_equal(pdf_width(output, numeric = TRUE), 8.5)
	expect_equal(pdf_height(output, numeric = TRUE), 11)
})
