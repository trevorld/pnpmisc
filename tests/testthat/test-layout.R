test_that("layout functions", {
	presets <- layout_names()
	expect_true(is.character(presets))

	expected_names <- c(
		"row",
		"col",
		"x",
		"y",
		"angle",
		"width",
		"height",
		"bleed",
		"paper",
		"orientation",
		"name"
	)

	df <- layout_preset("button_shy_cards")
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 6L)

	df <- layout_preset("button_shy_rules_2x2")
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 4L)

	df <- layout_grid(nrow = 1L, ncol = 1L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 1L)

	df <- layout_grid(nrow = 4L, ncol = 4L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 16L)

	df <- layout_grid(nrow = 5L, ncol = 5L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 25L)
})
