test_that("`pdf_add_origami()`", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pnpmisc:::pdf_create_mock_sbgj()
    f2 <- pdf_add_origami(f1)

    expect_equal(qpdf::pdf_length(f2), 1L)
    expect_equal(pdf_height(f2, numeric = TRUE), 8.5)
    expect_equal(pdf_width(f2, numeric = TRUE), 11)
})
