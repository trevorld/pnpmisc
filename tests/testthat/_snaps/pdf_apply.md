# `grid_fn` plus scaling

    Code
      f4 <- pdf_add_origami(f1, scale = 0.5)
    Message
      The original pdf contents were rasterized.
      i `tools::find_gs_cmd()` couldn't find a suitable `ghostscript`.
      i Suppress this message with `suppressMessages(expr, classes = "pnpmisc_message_rasterized")`.

# Backgrounds

    Code
      f5 <- pdf_apply(f1, bg = "#00008080", paper = "letter")
    Message
      The original pdf contents were rasterized.
      i `bg` is semi-transparent which our vector code cannot yet handle.
      i Suppress this message with `suppressMessages(expr, classes = "pnpmisc_message_rasterized")`.

# `pdf_apply()` warns when `bg` is ignored in vector path

    Code
      pdf_apply(f, bg = "blue", rasterize = FALSE)
    Condition
      Warning:
      `bg` is ignored in the vector path when not resizing.
      i Pass `paper` or `scale` to apply `bg`, or `rasterize = TRUE` to force rasterization.

# `pdf_apply()` errors when `isFALSE(rasterize)` but must rasterize

    Code
      pdf_apply(f, pages = "even", grid_fn = function() grid::grid.null(), rasterize = FALSE)
    Condition
      Error in `pdf_apply()`:
      ! `isFALSE(rasterize)` but the original pdf contents must be rasterized.
      i `any(pages != "all")` but `qpdf::pdf_overlay_stamp()` does not support a `pages` argument as of v1.4.1.

