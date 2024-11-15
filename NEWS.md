pnpmisc v0.1.0 (development)
============================

New features
------------

* The following pdf modification functions:

  + `pdf_add_bookmarks()`, `pdf_add_docinfo()`, and `pdf_add_xmp()` wrap around
    `{xmpdf}` functions to edit pdf metadata but follow the conventions of this package
    (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named) (#8).
  + `pdf_add_origami()` adds origami symbols to the pdf.
    Currently only supports adding some origami symbols to
    [Boardgame Barrio's Small Board Game Jackets](https://sites.google.com/view/boardgamebarrio/home) (#1).
  + `pdf_clean()` copies pdf file while removing temporary pdf files.
  + `pdf_compress()`, `pdf_rotate_pages()`, and `pdf_subset()` wrap the `{qpdf}` functions of the
    same name but follow the conventions of this package (i.e. input first positional argument, output second positional argument and defaults to temporary file, all other arguments must be named).
  + `pdf_gs()` processes a pdf file with ghostscript.
  + `pdf_pad_length()` adds blank pages to the end of a pdf file (#10).
  + `pdf_pad_pagesize()` makes a pdf file larger by padding it (i.e. adding space to the outside margins).
    The original images are **not** rescaled (#5).
  + `pdf_rm_crosshairs()` removes crosshairs from a pdf file.
    Currently only supports removing crosshairs from
    [Galdor's Grip](https://greggjewell.itch.io/galdors-grip) (PnP v1, letter) (#7).

Other utility functions:

  + `ls_temp_pdfs()` lists temporary pdfs while `rm_temp_pdfs()` removes temporary pdfs.
  + `pdf_blank()` creates blank pdf files.
  + `pdf_pages()` calculates an integer vector of subset of pdf pages (#9).
  + `pdf_width()` and `pdf_height()` get pdf page dimensions.

