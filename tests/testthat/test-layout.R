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

	df <- layout_preset("4x6_jacket")
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 1L)
	expect_equal(df$width, JACKET_4x6_WIDTH)
	expect_equal(df$height, JACKET_4x6_HEIGHT)

	df <- layout_preset("poker_jacket_1x1")
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 1L)
	expect_equal(df$width, JACKET_POKER_WIDTH)
	expect_equal(df$height, JACKET_POKER_HEIGHT)

	df <- layout_preset("poker_jacket_1x2")
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 2L)
	expect_equal(df$width, rep(JACKET_POKER_WIDTH, 2L))
	expect_equal(df$height, rep(JACKET_POKER_HEIGHT, 2L))
	expect_equal(df$bleed, c(0, 0))
	expect_equal(df$orientation, c("portrait", "portrait"))
	expect_equal(abs(diff(df$y)), JACKET_POKER_HEIGHT + 2 * JACKET_POKER_INNER_MARGIN)

	df <- layout_grid(nrow = 1L, ncol = 1L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 1L)

	df <- layout_grid(
		nrow = 2L,
		ncol = 2L,
		direction = "rtl",
		name = layout_name_fn("card_", width = 2L)
	)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 4L)
	expect_equal(df$name, paste0("card_0", c(2, 1, 4, 3)))

	df <- layout_grid(nrow = 4L, ncol = 4L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 16L)
	expect_equal(df$name, paste0("piece.", 1:16))

	df <- layout_grid(nrow = 5L, ncol = 5L)
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 25L)
})
