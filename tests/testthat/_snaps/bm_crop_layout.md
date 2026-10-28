# `bm_crop_layout()` error paths

    Code
      bm_crop_layout("not a pixmap", layout = "button_shy_cards")
    Condition
      Error in `bm_crop_layout()`:
      ! `page` must be a bm_pixmap object

---

    Code
      bm_crop_layout(page, layout = "button_shy_cards", row = 99L, col = 1L)
    Condition
      Error in `bm_crop_layout()`:
      ! `row`/`col` not found in `layout`

---

    Code
      bm_crop_layout(page, layout = "button_shy_cards", name = "no_such_name")
    Condition
      Error in `bm_crop_layout()`:
      ! `name` not found in `layout`

# `bm_crop_layout()` centers a preset name on the page's paper

    Code
      bm_crop_layout(special, layout = "poker_3x3")
    Condition
      Error in `bm_crop_layout()`:
      ! Couldn't infer a supported paper size from the aspect ratio of `page`.
      i To choose the paper size explicitly, pass a layout data frame e.g. `layout_preset("poker_3x3", paper = "a4")`.

