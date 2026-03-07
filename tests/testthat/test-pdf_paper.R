test_that("`pdf_paper()`", {
	on.exit(rm_temp_pdfs(), add = TRUE)
	f1 <- pdf_create_blank(paper = "legal", orientation = "landscape")
	expect_equal(pdf_paper(f1), "legal")

	f2 <- pdf_create_blank(paper = "a3", orientation = "portrait")
	expect_equal(pdf_paper(f2), "a3")

	f3 <- pdf_create_blank(paper = "a4", orientation = "landscape")
	expect_equal(pdf_paper(f3), "a4")

	f4 <- pdf_create_blank(paper = "a4", orientation = "portrait")
	expect_equal(pdf_paper(f4), "a4")

	f5 <- pdf_create_blank(paper = "poker", orientation = "landscape")
	expect_equal(pdf_paper(f5), "poker")

	f6 <- pdf_create_blank(paper = "bridge", orientation = "landscape")
	expect_equal(pdf_paper(f6), "bridge")

	f7 <- pdf_create_blank(width = 6, height = 4)
	expect_equal(pdf_paper(f7), "special")
})
