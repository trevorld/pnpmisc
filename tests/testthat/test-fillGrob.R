test_that("`grid.full()`", {
	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}
	draw_fill <- function(fill) {
		grid.newpage()
		grob <- grid.full(fill)
		expect_s3_class(grob, c("rect", "grob"))
	}
	pdf(NULL)
	on.exit(invisible(dev.off()), add = TRUE)
	draw_fill("blue")
	draw_fill(radialGradient())
	draw_fill(circleGrob(gp = gpar(col = NA, fill = "blue")))
	skip_if_not_installed("bittermelon")
	bm <- bittermelon::farming_crops_16x16()$avocado[[5L]]
	draw_fill(bm)
})
