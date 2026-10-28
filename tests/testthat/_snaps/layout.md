# layout functions

    Code
      layout_octavo(page = 3)
    Condition
      Error in `layout_octavo()`:
      ! `page` must be 1 or 2

---

    Code
      layout_octavo(signature = 0)
    Condition
      Error in `layout_octavo()`:
      ! `signature` must be a positive integer

---

    Code
      layout_octavo(bolt_padding = -1)
    Condition
      Error in `layout_octavo()`:
      ! `bolt_padding` must be non-negative

---

    Code
      layout_grid(direction = "up")
    Condition
      Error in `layout_grid()`:
      ! `direction` must be "left-to-right" or "right-to-left"

---

    Code
      layout_grid(nrow = 0L)
    Condition
      Error in `layout_grid()`:
      ! `nrow` must be a positive integer

---

    Code
      layout_grid(nrow = 2L, ncol = 2L, name = c("a", "b", "c"))
    Condition
      Error in `layout_grid()`:
      ! `name` must have length `nrow * ncol`

---

    Code
      layout_grid(nrow = 2L, ncol = 2L, name = c("a", "b", "b", "c"))
    Condition
      Error in `layout_grid()`:
      ! `name` must not contain duplicates

---

    Code
      layout_grid(nrow = 2L, ncol = 2L, direction = "rtl", angle = c(0, 90, 180))
    Condition
      Error in `layout_grid()`:
      ! `angle` must have length `nrow * ncol`

---

    Code
      layout_grid(paper = "foo")
    Condition
      Error in `layout_grid()`:
      ! `paper` must be one of "letter", "a4", "poker", "bridge", "legal", "executive", "a5", "a3".

---

    Code
      layout_grid(paper = "special")
    Condition
      Error in `layout_grid()`:
      ! `paper` must be one of "letter", "a4", "poker", "bridge", "legal", "executive", "a5", "a3".
      x `paper = "special"` has no fixed page size.

---

    Code
      layout_grid(2L, 2L, 2.5, 3.5, 0, "a4")
    Condition
      Error in `layout_grid()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "a4"
      i Did you forget to name an argument?

# `layout_grid()` infers `paper` from `input`

    Code
      layout_grid(input = a4, paper = "a4")
    Condition
      Error in `layout_grid()`:
      ! Can't pass both `input` and `paper`.
      i When `input` is passed `paper` is inferred from it with `pdf_paper()`.

---

    Code
      layout_grid(input = mixed)
    Condition
      Error in `layout_grid()`:
      ! The selected pages of `input` don't all have the same paper size.
      x Found "letter" and "a4" pages.
      i Either select pages with the same paper size via `pages` or pass `paper` instead of `input`.

---

    Code
      layout_grid(input = letter, pages = "even")
    Condition
      Error in `layout_grid()`:
      ! `pages` doesn't select any pages of `input`.

---

    Code
      layout_grid(pages = 2L)
    Condition
      Error in `layout_grid()`:
      ! Can't pass `pages` without `input`.
      i `pages` selects which pages of `input` to infer `paper` from.

---

    Code
      layout_grid(input = special)
    Condition
      Error in `layout_grid()`:
      ! Couldn't infer a supported paper size from the selected pages of `input`.
      i To choose the paper size explicitly, pass `paper` instead of `input`.

# `layout_from_pdf()` centers a preset on the input pdf's paper

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

