test_that("`pdf_create_blank()`", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    f <- pdf_create_blank(length = 4L)
    expect_equal(qpdf::pdf_length(f), 4L)
})

test_that("`pdf_pad_length()`", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    f <- pdf_create_blank(length = 1L)
    expect_equal(qpdf::pdf_length(f), 1L)

    f2 <- pdf_pad_length(f, minimum = 8L, multiples_of = 4L)
    expect_equal(qpdf::pdf_length(f2), 8L)

    f3 <- pdf_pad_length(f, minimum = 1L, multiples_of = 4L)
    expect_equal(qpdf::pdf_length(f3), 4L)

    f4 <- pdf_pad_length(f, minimum = 8L, multiples_of = 3L)
    expect_equal(qpdf::pdf_length(f4), 9L)

    f5 <- pdf_pad_length(f, minimum = 1L, multiples_of = 1L)
    expect_equal(qpdf::pdf_length(f5), 1L)
})
