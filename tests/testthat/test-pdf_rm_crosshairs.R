test_that("`pdf_rm_crosshairs()`", {
    skip_if_not_installed("bittermelon")
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
    f2 <- pdf_rm_crosshairs(f1, pages = "all")
    expect_equal(qpdf::pdf_length(f2), 2L)
})
