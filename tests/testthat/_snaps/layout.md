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

