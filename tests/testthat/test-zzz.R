test_that("`normalize_output()` resolves symlinks in the output directory", {
	skip_on_os("windows")
	real <- tempfile("real")
	dir.create(real)
	link <- tempfile("link")
	file.symlink(real, link)
	on.exit(unlink(c(link, real), recursive = TRUE), add = TRUE)
	expect_equal(
		normalize_output(file.path(link, "x.pdf")),
		file.path(normalizePath(real), "x.pdf")
	)
})
