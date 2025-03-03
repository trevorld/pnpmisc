test_that("`pdf_create_jacket()`", {
    skip_if_not_installed("piecepackr")
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pdf_create_jacket()
    expect_equal(qpdf::pdf_length(f1), 1L)
    
    f2 <- pdf_create_jacket(front = "red", back = "blue",
                            spine = "green", inner = "grey")
    expect_equal(qpdf::pdf_length(f2), 2L)

    f3 <- pdf_create_jacket(front = nullGrob(), back = nullGrob(),
                            spine = nullGrob(), inner = nullGrob())
    expect_equal(qpdf::pdf_length(f3), 2L)
})
