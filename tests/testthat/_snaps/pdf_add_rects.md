# `pdf_add_rects()` centers a preset name on the input pdf's paper

    Code
      pdf_add_rects(mixed, layout = "poker_3x3")
    Condition
      Error in `pdf_add_rects()`:
      ! The selected pages of `input` don't all have the same paper size.
      x Found "letter" and "a4" pages.
      i Either select pages with the same paper size via `pages` or pass a layout data frame e.g. `layout_preset("poker_3x3", paper = "a4")`.

---

    Code
      pdf_add_rects(special, layout = "poker_3x3")
    Condition
      Error in `pdf_add_rects()`:
      ! Couldn't infer a supported paper size from the selected pages of `input`.
      i To choose the paper size explicitly, pass a layout data frame e.g. `layout_preset("poker_3x3", paper = "a4")`.

