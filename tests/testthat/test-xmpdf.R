test_that("{xmpdf} wrappers", {
    skip_if_not_installed("xmpdf")

    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pdf_blank(length = 2L)

    skip_if_not(xmpdf::supports_set_bookmarks())
    bm <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
    f2 <- pdf_add_bookmarks(f1, bookmarks = bm)
    expect_equal(qpdf::pdf_length(f2), 2L)

    skip_if_not(xmpdf::supports_set_docinfo())
    di <- xmpdf::docinfo(title = "A Title", author = "The Author")
    f3 <- pdf_add_docinfo(f1, docinfo = di)
    expect_equal(qpdf::pdf_length(f3), 2L)

    skip_if_not(xmpdf::supports_set_xmp())
    xmp <- xmpdf::xmp(title = "A Title", creator = "The Author")
    f4 <- pdf_add_xmp(f1, xmp = xmp)
    expect_equal(qpdf::pdf_length(f4), 2L)
})
