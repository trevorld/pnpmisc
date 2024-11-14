test_that("miscellaneous functions", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    skip_if(tools::find_gs_cmd()[[1L]] == "")
    
    f1 <- tempfile(fileext = ".pdf")
    grDevices::pdf(f1, width = 8.5, height = 11)
    grid::grid.text("")
    invisible(grDevices::dev.off())
    
    f2 <- pdf_gs(f1)

    expect_equal(qpdf::pdf_length(f2), 1L)
    expect_equal(pdf_height(f2, numeric = TRUE), 11)
    expect_equal(pdf_width(f2, numeric = TRUE), 8.5)

    expect_equal(pdf_height(f2, units = "bigpts", numeric = TRUE), 11 * 72)
    expect_equal(pdf_width(f2, units = "bigpts", numeric = TRUE), 8.5 * 72)

    expect_true(file.exists(f2))
    f3 <- pdf_clean(f2)
    expect_false(file.exists(f2))
    expect_true(file.exists(f3))

    f4 <- pdf_compress(f3)
    f5 <- pdf_rotate_pages(f4, angle = 90)
    f6 <- pdf_subset(f5, pages = 1L)
    f7 <- pdf_gs(f6)
    expect_equal(qpdf::pdf_length(f7), 1L)
    expect_equal(pdf_height(f7, numeric = TRUE), 8.5)
    expect_equal(pdf_width(f7, numeric = TRUE), 11)
})
