test_that("`pdf_create_wallet()`", {
    on.exit(rm_temp_pdfs(), add = TRUE)

    f1 <- pdf_create_wallet()
    expect_equal(qpdf::pdf_length(f1), 2L)
    
    f2 <- pdf_create_wallet(front = "red", back = "blue", spine = "green", 
                            bleed = TRUE)
    expect_equal(qpdf::pdf_length(f2), 2L)

    f3 <- pdf_create_wallet(front = nullGrob(), back = nullGrob(), 
                            spine = nullGrob(), bleed = TRUE)
    expect_equal(qpdf::pdf_length(f3), 2L)
})
