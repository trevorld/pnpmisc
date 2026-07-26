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

	df <- layout_octavo()
	expect_true(is.data.frame(df))
	expect_true(all(hasName(df, expected_names)))
	expect_equal(nrow(df), 8L)
	expect_equal(df$name, paste0("page_", c(5, 12, 9, 8, 4, 13, 16, 1)))
	expect_equal(df$angle, rep(c(180, 0), each = 4L))

	df <- layout_octavo(page = 2)
	expect_equal(df$name, paste0("page_", c(7, 10, 11, 6, 2, 15, 14, 3)))

	df <- layout_octavo(signature = 2)
	expect_equal(df$name, paste0("page_", c(5, 12, 9, 8, 4, 13, 16, 1) + 16))

	df <- layout_octavo(page = 2, signature = 3)
	expect_equal(df$name, paste0("page_", c(7, 10, 11, 6, 2, 15, 14, 3) + 32))

	all_pages <- sort(as.integer(sub(
		"page_",
		"",
		c(
			layout_octavo()$name,
			layout_octavo(page = 2)$name
		)
	)))
	expect_equal(all_pages, 1:16)

	expect_snapshot(error = TRUE, layout_octavo(page = 3))
	expect_snapshot(error = TRUE, layout_octavo(signature = 0))
	expect_snapshot(error = TRUE, layout_octavo(bolt_padding = -1))

	df0 <- layout_octavo()
	df <- layout_octavo(bolt_padding = 0.2)
	expect_equal(df$width, df0$width)
	expect_equal(df$height, df0$height)
	# uncut two-page spreads stay flush (no added gap)
	expect_equal(df$x[2] - df$x[1], df$width[1])
	expect_equal(df$x[4] - df$x[3], df$width[1])
	expect_equal(df$x[6] - df$x[5], df$width[1])
	expect_equal(df$x[8] - df$x[7], df$width[1])
	# folds that get trimmed open gain the extra bolt_padding
	expect_equal(df$x[3] - df$x[2], df$width[1] + 0.2)
	expect_equal(df$x[7] - df$x[6], df$width[1] + 0.2)
	expect_equal(df$y[1] - df$y[5], df$height[1] + 0.2)

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

	expect_snapshot(error = TRUE, layout_grid(direction = "up"))
	expect_snapshot(error = TRUE, layout_grid(nrow = 0L))
	expect_snapshot(error = TRUE, layout_grid(nrow = 2L, ncol = 2L, name = c("a", "b", "c")))
	expect_snapshot(error = TRUE, layout_grid(nrow = 2L, ncol = 2L, name = c("a", "b", "b", "c")))
	expect_snapshot(
		error = TRUE,
		layout_grid(nrow = 2L, ncol = 2L, direction = "rtl", angle = c(0, 90, 180))
	)
})
