#' N-up pages of a document with `pdfxup`
#'
#' `pdf_pdfxup()` creates an n-up pdf using the `pdfxup` command-line program.
#' `pdf_pdfxup_booklet()` is a convenience wrapper around `pdf_pdfxup()` with
#' opinionated defaults better suited for printing a physical booklet on a home printer.
#' `pdfxup` computes the bounding box of the input to remove margins, keeping text readable.
#'
#' `pdf_pdfxup()` tries to closely follow the `pdfxup` defaults.  In contrast `pdf_pdfxup_booklet()` makes these alternative defaults:
#'
#' * `booklet = "se"` which I find easier to reason about than `"le"`
#' * `margins = unit(3/8, "in")` to provide adequate margins for home printers
#' * `intspaces = unit(3/16, "in")` to provide adequate gutter space for folding
#' * `innermargins = unit(0, "pt")`
#'
#' @inheritParams pdf_apply
#' @param ... Ignored.
#' @param ncol,nrow Number of columns/rows per page.
#' @param byrow If `TRUE` fill pages row by row. If `FALSE` fill pages column by column.
#' @param booklet If `TRUE` configure for printing as a 2-up booklet.
#'   Can also be `"se"` (short-edge mode) or `"le"` (long-edge mode) for explicit control over two-sided printing; `booklet = TRUE` defaults to "le" mode.
#' @param orientation Either `"landscape"` or `"portrait"`.
#' @param paper Paper size (e.g. `"letter"` or `"a4"`).
#'   The name must be known by the LaTeX `geometry` package.
#' @param margins Margin of pages of the new document as a [grid::unit()] object.
#' @param intspaces Space between pages as a [grid::unit()] object.
#' @param innermargins Inner margin between frame and page as a [grid::unit()] object.
#' @param framewidth Width of the frame around each page as a [grid::unit()] object.
#' @param default.units Default units when a numeric value is passed for
#'   `margins`, `intspaces`, `innermargins`, or `framewidth`.
#' @param clip If `TRUE` clip pages to the computed bounding box.
#' @param bb_pages Pages to use when computing the bounding box (see `pages` argument for supported values).
#'   Defaults to `"all"`.  Note negative numbers indicate pages to omit from computation of bounding box.
#' @return `output` pdf file name invisibly.
#'         As a side effect creates an n-up pdf.
#' @seealso <https://ctan.org/tex-archive/support/pdfxup> for more information about `pdfxup`
#' @examples
#' f1 <- system.file("doc", "Sweave.pdf", package = "utils")
#' if (nzchar(Sys.which("pdfxup")) && file.exists(f1)) {
#'   f2 <- pdf_pdfxup_booklet(f1)
#'   unlink(f2)
#' }
#' @export
pdf_pdfxup <- function(
	input,
	output = NULL,
	...,
	ncol = 2L,
	nrow = 1L,
	byrow = TRUE,
	booklet = FALSE,
	orientation = "landscape",
	paper = getOption("papersize", "letter"),
	margins = unit(5, "pt"),
	intspaces = unit(1, "pt"),
	innermargins = unit(5, "pt"),
	framewidth = ifelse(isFALSE(booklet), unit(0.4, "pt"), unit(0.0, "pt")),
	default.units = "pt",
	clip = TRUE,
	pages = "all",
	bb_pages = "all"
) {
	check_dots_empty()
	cmd <- Sys.which("pdfxup")
	stopifnot(nzchar(cmd))
	input <- normalizePath(input)
	output <- normalize_output(output, input)

	v <- pdfxup_version()
	if (isFALSE(byrow) && v < "2.10") {
		abort(sprintf("`byrow = FALSE` requires pdfxup >= 2.10 but detected version %s.", v))
	}
	if (isFALSE(clip) && v < "1.50") {
		abort(sprintf("`clip = FALSE` requires pdfxup >= 1.50 but detected version %s.", v))
	}
	as_pt <- as_pt_fn(default.units, v)

	# fmt: skip
	args <- c(
		"--columns", as.character(ncol),
		"--rows", as.character(nrow),
		"--paper", paper,
		"--margins", as_pt(margins),
		"--intspaces", as_pt(intspaces),
		"--innermargins", as_pt(innermargins),
		"--framewidth", as_pt(framewidth),
		ifelse(orientation == "landscape", "--landscape", "--portrait"),
		if (v >= "2.10") ifelse(isFALSE(byrow), "--vertical", "--horizontal"),
		if (v >= "1.50") ifelse(isFALSE(clip), "--no-clip", "--clip")
	)

	if (!isFALSE(booklet)) {
		booklet_val <- ifelse(isTRUE(booklet), "le", booklet)
		stopifnot(booklet_val %in% c("le", "se"))
		args <- c(args, "--booklet", booklet_val)
	}

	if (!identical(bb_pages, "all")) {
		bb_pages <- pdf_pages(input, pages = bb_pages)
		args <- c(args, "--bb", paste(bb_pages, collapse = ","))
	}

	if (!identical(pages, "all")) {
		pages <- pdf_pages(input, pages = pages)
		args <- c(args, "--pages", paste(pages, collapse = ","))
	}

	args <- c(args, "-ow", "-o", shQuote(output), shQuote(input))
	system2_cmd(cmd, args)

	invisible(output)
}

pdfxup_version <- function() {
	cmd <- Sys.which("pdfxup")
	stopifnot(nzchar(cmd))
	output <- system2_cmd(cmd, "--version")
	numeric_version(sub("^pdfxup version (\\S+).*", "\\1", output))
}

#' @rdname pdf_pdfxup
#' @export
pdf_pdfxup_booklet <- function(
	input,
	output = NULL,
	...,
	booklet = "se",
	margins = unit(0.375, "in"),
	intspaces = unit(0.1875, "in"),
	innermargins = unit(0, "pt")
) {
	pdf_pdfxup(
		input,
		output,
		...,
		booklet = booklet,
		margins = margins,
		intspaces = intspaces,
		innermargins = innermargins
	)
}

as_pt_fn <- function(default.units, v) {
	# v2.11 (2024/03/15) is when fractional values were first supported
	fmt_dim <- ifelse(v >= "2.11", "%.1fpt", "%.0fpt")
	as_unit <- function(x) if (is.numeric(x)) unit(x, default.units) else x
	fmt_pt <- function(x) sprintf(fmt_dim, x)
	function(x) {
		if (dev.cur() == 1L) {
			grDevices::pdf(NULL)
			on.exit(invisible(dev.off()), add = TRUE)
		}
		as_unit(x) |>
			convertUnit("pt", valueOnly = TRUE) |>
			fmt_pt()
	}
}
