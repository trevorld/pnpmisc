pnpmisc v0.1.0 (development)
============================

New features
------------

* The following PDF modification functions:

  + `pdf_add_origami()` adds origami symbols to the pdf.
    Currently only supports adding some origami symbols to
    [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home) (#1).
  + `pdf_clean()` copies pdf file while removing temporary pdf files.
  + `pdf_compress()`, `pdf_rotate_pages()`, and `pdf_subset()` wrap the `{qpdf}` functions of the
    same name but follow the conventions of this package (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named).
  + `pdf_gs()` processes a pdf file with ghostscript.
  + `pdf_append_blank()` adds blank pages to the end of a pdf file (#10).
  + `pdf_pad_paper()` makes a pdf file larger by padding it (i.e. adding space to the outside margins).
    The original images are **not** rescaled (#5).
  + `pdf_rm_crosshairs()` removes crosshairs from a pdf file.
    Currently only supports removing crosshairs from
    [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP v1, letter) (#7).
  + `pdf_set_bookmarks()`, `pdf_set_docinfo()`, and `pdf_set_xmp()` wrap around
    `{xmpdf}` functions to edit pdf metadata but follow the conventions of this package
    (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named) (#8).

* The following PDF creation functions:

  + `pdf_create_blank()` creates blank pdf files.
  + `pdf_create_jacket()` creates 4x6 photo box jacket pdf files (#16).
  + `pdf_create_wallet()` creates origami wallet pdf files (#4).

* Other PDF utility functions:

  + `ls_temp_pdfs()` lists temporary pdfs while `rm_temp_pdfs()` removes temporary pdfs.
  + `pdf_orientation()` tells whether a pdf is in portrait or landscape mode.
  + `pdf_pages()` calculates an integer vector of subset of pdf pages (#9).
  + `pdf_render_raster()` renders a PDF page as a `raster` or `nativeRaster` object.
  + `pdf_width()` and `pdf_height()` get pdf page dimensions.

* `{bittermelon}` pixmap object functions:

  + `pdf_render_bm_pixmap()` renders a PDF page as a `bittermelon::bm_pixmap()` object.
