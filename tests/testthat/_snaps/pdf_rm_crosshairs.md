# `pdf_rm_crosshairs()`

    Code
      f3 <- pdf_rm_crosshairs(f2, pages = "odd", layout = "poker_3x2_bleed", dpi = 75)
    Message
      The original pdf contents were rasterized.
      i We needed to edit the contents with a bitmap function.`
      i Suppress this message with `suppressMessages(expr, classes = "pnpmisc_message_rasterized")`.

# `bm_rm_crosshairs()` centers a preset name on the bitmap's paper

    Code
      bm_rm_crosshairs(special, layout = "poker_3x2_bleed")
    Condition
      Error in `bm_rm_crosshairs()`:
      ! Couldn't infer a supported paper size from the aspect ratio of `x`.
      i To choose the paper size explicitly, pass a layout data frame e.g. `layout_preset("poker_3x2_bleed", paper = "a4")`.

