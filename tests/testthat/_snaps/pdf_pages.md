# `pdf_pages()`

    Code
      pdf_pages(f3, pages = "interleave")
    Condition
      Error in `pages_interleave()`:
      ! "interleave" requires an even number of pages but got 3.

---

    Code
      pdf_pages(f3, pages = "2-up saddle stitch")
    Condition
      Error in `pages_2up_saddle_stitch()`:
      ! "2-up saddle stitch" requires a number of pages divisible by 4 but got 3.

---

    Code
      pdf_pages(f3, pages = list())
    Condition
      Error in `pdf_pages()`:
      ! `pages` must be numeric or character

---

    Code
      pdf_pages(f3, pages = 4L)
    Condition
      Error in `pdf_pages()`:
      ! `pages` must only select pages that exist.
      i `input` has 3 pages.

