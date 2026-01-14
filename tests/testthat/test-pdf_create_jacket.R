test_that("`pdf_create_jacket()`", {
	skip_if_not_installed("piecepackr")
	on.exit(rm_temp_pdfs(), add = TRUE)
	op <- options(papersize = "letter")
	on.exit(options(op), add = TRUE)

	f1 <- pdf_create_4x6_jacket()
	expect_equal(qpdf::pdf_length(f1), 1L)
	expect_equal(pdf_paper(f1), "letter")

	f2 <- pdf_create_4x6_jacket(front = "red", back = "blue", spine = "green", inner = "grey")
	expect_equal(qpdf::pdf_length(f2), 2L)
	expect_equal(pdf_paper(f2), c("letter", "letter"))

	f3 <- pdf_create_poker_jacket(
		front = nullGrob(),
		back = nullGrob(),
		spine = nullGrob(),
		inner = nullGrob()
	)
	expect_equal(qpdf::pdf_length(f3), 2L)

	f4 <- pdf_create_poker_jacket(
		front = list(nullGrob(), nullGrob()),
		back = nullGrob(),
		spine = nullGrob(),
		inner = nullGrob()
	)
	expect_equal(qpdf::pdf_length(f4), 2L)

	f5 <- pdf_create_jacket_instructions()
	expect_equal(qpdf::pdf_length(f5), 1L)
})
