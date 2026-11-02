# Evaluates `code` with two (null pdf) devices open and the second one active
# (so closing a temporary device without restoring would wrap around to the first one),
# then expects that the second one is active again and no device was opened or closed.
# Wrap `code` in `try()` to check the devices after an error.
expect_devices_restored <- function(code) {
	old <- grDevices::dev.cur()
	grDevices::pdf(NULL)
	d1 <- grDevices::dev.cur()
	grDevices::pdf(NULL)
	d2 <- grDevices::dev.cur()
	on.exit(close_temp_dev(d2, old), add = TRUE)
	on.exit(close_temp_dev(d1, old), add = TRUE)
	before <- grDevices::dev.list()
	force(code)
	expect_equal(grDevices::dev.list(), before)
	expect_equal(grDevices::dev.cur(), d2)
}
