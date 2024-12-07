test_that("`pdf_rm_crosshairs()`", {
    skip_if_not_installed("bittermelon")
    skip_if_not_installed("piecepackr", "1.14.0-5")
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pdf_create_blank(length = 2L, width = 11, height = 8.5)
    f2 <- pdf_add_crosshairs(f1, pages = "all",
                             layout = "poker_3x2_bleed", dpi = 75)
    f3 <- pdf_rm_crosshairs(f2, pages = "odd",
                            layout = "poker_3x2_bleed", dpi = 75)
    expect_equal(qpdf::pdf_length(f2), 2L)
    expect_equal(qpdf::pdf_length(f3), 2L)

    f4 <- pdf_add_cropmarks(f2, pages = "all",
                            layout = "poker_3x2_bleed", dpi = 75)
    expect_equal(qpdf::pdf_length(f4), 2L)
})
