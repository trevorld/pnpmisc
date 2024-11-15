test_that("`pdf_rm_crosshairs()`", {
    skip_if_not_installed("bittermelon")
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- tempfile(fileext = ".pdf")
    grDevices::pdf(f1, width = 11, height = 8.5)
    grid::grid.newpage()
    grid::grid.text("")
    grid::grid.newpage()
    grid::grid.text("")
    invisible(grDevices::dev.off())

    f2 <- pdf_rm_crosshairs(f1, pages = "all")
    expect_equal(qpdf::pdf_length(f2), 2L)
})
