test_that("`with_temp_dev()` returns the value of `code` on the temporary device", {
	expect_devices_restored({
		before <- grDevices::dev.list()
		value <- with_temp_dev(\() grDevices::pdf(NULL), grDevices::dev.cur())
		expect_length(intersect(value, before), 0L)
	})
})

test_that("`with_temp_dev()` restores the devices when `code` or `open()` errors", {
	expect_devices_restored(try(
		with_temp_dev(\() grDevices::pdf(NULL), stop("boom")),
		silent = TRUE
	))
	open <- function() {
		grDevices::pdf(NULL)
		stop("boom")
	}
	expect_devices_restored(try(with_temp_dev(open, 1), silent = TRUE))
})

test_that("`with_temp_dev()` leaves no device open if none was open before", {
	skip_if(grDevices::dev.cur() != 1L, "a graphics device is already open")
	expect_equal(with_temp_dev(\() grDevices::pdf(NULL), 2), 2)
	expect_null(grDevices::dev.list())
})

test_that("`with_temp_dev()` errors if `open()` doesn't open a device", {
	expect_snapshot(error = TRUE, with_temp_dev(\() NULL, 1))
})

test_that("`close_temp_dev()` can be called more than once", {
	expect_devices_restored({
		old <- grDevices::dev.cur()
		grDevices::pdf(NULL)
		temp <- grDevices::dev.cur()
		close_temp_dev(temp, old)
		close_temp_dev(temp, old)
	})
})
