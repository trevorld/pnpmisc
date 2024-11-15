test_that("`pdf_pad_pagesize()`", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    # portrait
    input <- pdf_blank(width = 8.3, height = 11, bg = "blue")
    expect_equal(pdf_width(input, numeric = TRUE), 8.3, tolerance = 0.01)
    expect_equal(pdf_height(input, numeric = TRUE), 11)

    output <- pdf_pad_pagesize(input)
    expect_equal(pdf_width(output, numeric = TRUE), 8.5)
    expect_equal(pdf_height(output, numeric = TRUE), 11)

    output_a4 <- pdf_pad_pagesize(input, paper_size = "A4")
    expect_equal(pdf_width(output_a4, numeric = TRUE), 8.3, tolerance = 0.01)
    expect_equal(pdf_height(output_a4, numeric = TRUE), 11.7, tolerance = 0.01)

    # landscape
    input <- pdf_blank(width = 11, height = 8.3, bg = "blue")
    expect_equal(pdf_width(input, numeric = TRUE), 11)
    expect_equal(pdf_height(input, numeric = TRUE), 8.3, tolerance = 0.01)

    output <- pdf_pad_pagesize(input)
    expect_equal(pdf_width(output, numeric = TRUE), 11)
    expect_equal(pdf_height(output, numeric = TRUE), 8.5)

    output_a4 <- pdf_pad_pagesize(input, paper_size = "A4")
    expect_equal(pdf_width(output_a4, numeric = TRUE), 11.7, tolerance = 0.01)
    expect_equal(pdf_height(output_a4, numeric = TRUE), 8.3, tolerance = 0.01)
})
