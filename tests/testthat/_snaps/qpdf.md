# `pdf_overlay_stamp()` with `pages`

    Code
      pdf_overlay_stamp(f1, stamp = stamp, pages = "odd")
    Condition
      Error in `pdf_overlay_stamp()`:
      ! `pages` must select every page because the installed `qpdf::pdf_overlay_stamp()` does not support a `pages` argument (requires a version of `qpdf` newer than v1.4.1).

---

    Code
      pdf_overlay_stamp(f1, stamp = stamp, pages = integer(0))
    Condition
      Error in `pdf_overlay_stamp()`:
      ! `pages` must select at least one page.

---

    Code
      pdf_subset(f1, pages = integer(0))
    Condition
      Error in `pdf_subset()`:
      ! `pages` must select at least one page.

