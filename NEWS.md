pnpmisc v0.2.2
==============

Breaking changes
----------------

* The first argument of `layout_preset()` is now `preset` instead of `name` (so we can now pass `layout_grid()`'s new `name` argument in the `...`).
* The default argument for `paper` is now `getOption("papersize", "letter")` instead of (depending on the function) "letter" or "special".
* `bm_create_pdf()` and `pdf_create_blank()` now follow the convention that paper size will be determined by `height` and `width` if either are non-missing (and/or `paper = "special"`) otherwise paper size will be determined by both `paper` and `orientation`.
* `layout_grid()` now requires the arguments after `bleed` (i.e. `paper`, `orientation`, `angle`, `name`, and `direction`) to be named.
* `ls_temp_pdfs()` and `rm_temp_pdfs()` now only list/remove the temporary pdf files created by this package (which now use a `___pnpmisc.pdf` file extension) instead of every pdf file in `tempdir()`.  Pass `fileext = ".pdf"` for the previous behaviour (#67).
* By default `pdf_rm_crosshairs()` now removes crosshairs from all pages.  You'll need to explicitly pass `pages = "odd"` to get the previous behaviour.
* The original pdf contents are now rasterized in fewer cases.  In such cases if you want to explicitly rasterize the pdf contents you'll need to pass `rasterize = TRUE`.

New features
------------

* The "executive" paper size (7.25 by 10.5 inches) is now supported wherever a `paper` argument is accepted and is now inferred by `pdf_paper()`.
* `layout_grid()` gains `input` and `pages` arguments to center the layout on the paper size of the input pdf's (selected) pages as inferred by `pdf_paper()` instead of on `getOption("papersize")`.  Since `layout_preset()` and `layout_octavo()` forward `...` to `layout_grid()` they can also be passed `input` and `pages` (#70).
* The following PDF creation functions:

  + `pdf_add_lines()` adds lines along the components of a print-and-play layout.
  + `pdf_add_overlay()` overlays custom graphics on selected pages.
  + `pdf_apply()` can modify the selected pages of pdf files by overlaying custom graphics on top and/or customizing the (rasterized) original contents.
  + `pdf_change_paper()` changes the paper size of a pdf centering the original contents.
  + `pdf_combine()` combines multiple pdf files into one (wraps `qpdf::pdf_combine()`).
  + `pdf_create_4x6_jacket()` creates 4x6 photo storage box jacket pdf files.
  + `pdf_create_poker_jacket()` creates poker deck storage box jacket pdf files.
  + `pdf_overlay_stamp()` overlays a stamp pdf onto the pages of a pdf (wraps `qpdf::pdf_overlay_stamp()`).
  + `pdf_pdfxup()` wraps the `pdfxup` command-line program to create n-up PDF documents with automatic margin removal. `pdf_pdfxup_booklet()` is a convenience wrapper with defaults suited for printing a physical booklet.
  + `pdf_rasterize()` re-renders a pdf by rasterizing each page.
  + `pdf_rep()` repeats pages of a pdf similar to `rep()`.
  + `pdf_scale()` scales pdf contents centered within the page.

* The following enhancements to `pdf_pages()`:

  + `pdf_pages()` gains three new `pages` string options: `"interleave"` interleaves the first and second halves of the pages (requires an even number of pages); `"interleave_first"` repeats the first page and interleaves it with the remaining pages; `"interleave_last"` repeats the last page and interleaves it with the preceding pages.
  + `pdf_pages()` now throws an error if a numeric `pages` selects a page that doesn't exist.
  + `pdf_pages()` can now return an empty selection (e.g. `pages = "even"` on a one-page pdf).

* `pdf_add_cropmarks()`, `pdf_add_crosshairs()`, `pdf_add_lines()`, `pdf_add_rects()`, and `pdf_rm_crosshairs()` now center a layout preset name passed to `layout` on the paper size of the input pdf's pages (as inferred by `pdf_paper()`) instead of on `getOption("papersize")` and throw an error if those pages don't share a supported paper size (#70).
* `pdf_create_jacket()` gains `orientation`, `width`, `height`, `depth`, and `bg` arguments.
* `pdf_create_jacket_instructions()` gains an `orientation` argument.

* Other PDF utilities:

 + `pdf_paper()` infers the paper size of a PDF document (#42).

* `{bittermelon}` pixmap object functions:

  + `bm_create_pdf()` creates pdf documents from a list of (raster) images (#36).
  + `bm_paper()` infers the paper size of a bitmap (or each bitmap in a `bittermelon::bm_list()`) from its aspect ratio or, if `dpi` is passed, from its size (#70).
  + `bm_rm_crosshairs()` removes crosshairs from a raster object.
  + `bm_split_layout()` extracts all the components in a page of a print-and-play layout into a `bittermelon::bm_list()` of `bittermelon::bm_pixmap()` objects.
  + `pdf_render_bm_list()` renders all the pages in a pdf into bittermelon pixmap objects (#35).
  + `zip_extract_bm_pixmap()` extracts/renders an image from a zipfile into a bittermelon pixmap object.

* `{grid}` functions:

  + `as_fill()` converts bitmap or grob objects to a `grid::pattern()`
    to be used as fills in grid grobs.
  + `fullGrob()` is a wrapper around `grid::rectGrob()` for a single
    rectangle grob filled with a color, bitmap, grob, or pattern.
    `grid.full()` creates a `fullGrob()` and draws it to the active graphics device.
  + `grid_add_layout()` draws a list of images according to a layout object.
  + `grid_add_origami()` draws origami fold symbols for a box jacket. `grid_add_origami_bb()` draws origami fold symbols optimized for Boardgame Barrio's Small Board Game Jackets.

* The following enhancements to `bm_crop_layout()`:

  + Adds `name` argument as an alternative to `row` and `col`.
  + Will now rotate the returned pixmap "up" if the `layout`'s `angle` value is non-zero.

* The following enhancements to `layout_grid()`:

  + Adds `angle`, `direction`, and `name` arguments
  + Adds `angle` and `name` columns to returned data frames.

* The following enhancements to `layout_preset()`:

  + New `...` argument(s) passed to `layout_grid()`.
  + New supported `name` value `button_shy_rules_2x2` to support Button Shy's 4-page booklets.
  + New supported `name` values `4x6_jacket`, `poker_jacket_1x1`, and `poker_jacket_1x2` for storage box jackets created by `pdf_create_4x6_jacket()` and `pdf_create_poker_jacket()`.
  + Adds `angle` and `name` columns to returned data frames.

* `layout_name_fn()` can be used to return a function to generate layout names in `layout_grid()`'s `name` argument.

* `layout_octavo()` calculates a layout data frame for one side of a 16-page octavo booklet signature using a standard sheetwise imposition.

* `ls_temp_pdfs()`, `rm_temp_pdfs()`, and `pdf_clean()` gain a `fileext` argument to control which temporary pdf files are listed/removed (#67).

* All functions with a `dpi` argument now default to `getOption("pnpmisc.dpi", 300)` to allow setting a global default dpi.

Bug fixes
---------

* `pdf_width()` and `pdf_height()` no longer leave an `Rplots.pdf` file behind in the working directory when called with no graphics device open.  This also affected the functions that call them internally such as `pdf_append_blank()`, `pdf_change_paper()`, and `pdf_scale()` (#67).

pnpmisc v0.1.1
==============

New features
------------

* The following PDF modification functions:

  + `pdf_add_cropmarks()` adds crop marks to the pdf (#21).
  + `pdf_add_crosshairs()` adds crosshairs to the pdf (#2).
  + `pdf_add_origami()` adds origami symbols to the pdf.
    Currently only supports adding some origami symbols to
    [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home) (#1).
  + `pdf_add_rects()` adds (round)rects to the pdf (#6).
  + `pdf_clean()` copies pdf file while removing temporary pdf files.
  + `pdf_compress()`, `pdf_rotate_pages()`, and `pdf_subset()` wrap the `{qpdf}` functions of the
    same name but follow the conventions of this package (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named).
  + `pdf_gs()` processes a pdf file with ghostscript.
  + `pdf_append_blank()` adds blank pages to the end of a pdf file (#10).
  + `pdf_pad_paper()` makes a pdf file larger by padding it (i.e. adding space to the outside margins).
    The original images are **not** rescaled (#5).
  + `pdf_rm_crosshairs()` removes crosshairs from a pdf file (#7).
  + `pdf_set_bookmarks()`, `pdf_set_docinfo()`, and `pdf_set_xmp()` wrap around
    `{xmpdf}` functions to edit pdf metadata but follow the conventions of this package
    (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named) (#8).

* The following PDF creation functions:

  + `pdf_create_blank()` creates blank pdf files.
  + `pdf_create_jacket()` creates 4x6 photo box jacket pdf files (#16).
    `pdf_create_jacket_instructions()` creates a printable sheet of
    instructions for making such a jacket.
  + `pdf_create_wallet()` creates origami wallet pdf files (#4).

* Other PDF utility functions:

  + `ls_temp_pdfs()` lists temporary pdfs while `rm_temp_pdfs()` removes temporary pdfs.
  + `pdf_orientation()` tells whether a pdf is in portrait or landscape mode.
  + `pdf_pages()` calculates an integer vector of subset of pdf pages (#9).
  + `pdf_render_raster()` renders a PDF page as a `raster` or `nativeRaster` object.
  + `pdf_width()` and `pdf_height()` get pdf page dimensions.

* Layout data frame functions:

  + `layout_grid()` calculates a layout data frame
    for a grid of identically sized print-and-play components.
  + `layout_preset()` calculates a layout data frame for a named preset.
  + `layout_names()` returns the supported layout presets.

* `{bittermelon}` pixmap object functions:

  + `pdf_render_bm_pixmap()` renders a PDF page as a `bittermelon::bm_pixmap()` object.
  + `bm_crop_layout()` crops out a print-and-play component from a layout.

* `{grid}` functions:

  + `grid_add_cropmarks()` draws crop marks around components in a layout.
  + `grid_add_crosshairs()` draws crosshairs around components in a layout.
  + `grid_add_lines()` draws along the edges of components in a layout.
  + `grid_add_rects()` draws (rounded) rectangles around components in a layout.
