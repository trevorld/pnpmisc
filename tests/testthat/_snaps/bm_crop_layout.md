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

