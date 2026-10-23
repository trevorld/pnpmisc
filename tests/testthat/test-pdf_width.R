test_that("`pdf_width()` doesn't write to the working directory", {
	skip_if(dev.cur() != 1L, "a graphics device is already open")
	on.exit(rm_temp_pdfs(), add = TRUE)
	f <- pdf_create_blank(width = 8.5, height = 11)

	dir <- tempfile()
	dir.create(dir)
	wd <- setwd(dir)
	on.exit(setwd(wd), add = TRUE)
	on.exit(unlink(dir, recursive = TRUE), add = TRUE)

	expect_equal(pdf_width(f, numeric = TRUE), 8.5)
	expect_equal(pdf_height(f, numeric = TRUE), 11)
	expect_equal(list.files(dir), character(0L))
})
