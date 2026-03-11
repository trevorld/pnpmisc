# `pdf_apply()` errors when `isFALSE(rasterize)` but must rasterize

    Code
      pdf_apply(f, pages = "even", grid_fn = function() grid::grid.null(), rasterize = FALSE)
    Condition
      Error in `pdf_apply()`:
      ! `isFALSE(rasterize)` but the original pdf contents must be rasterized.
      i `any(pages != "all")` but `qpdf::pdf_overlay_stamp()` does not support a `pages` argument as of v1.4.1.

