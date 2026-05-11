# `grid_add_layout()` errors on unsupported image type

    Code
      grid_add_layout(list(a = "not an image"), layout = layout)
    Condition
      Error in `grid_add_layout()`:
      ! Image "a" must be a supported bitmap or grob but got class "character".

