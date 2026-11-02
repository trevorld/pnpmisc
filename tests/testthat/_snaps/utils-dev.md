# `with_temp_dev()` errors if `open()` doesn't open a device

    Code
      with_temp_dev(function() NULL, 1)
    Condition
      Error in `with_temp_dev()`:
      ! `open()` did not open a new graphics device.

