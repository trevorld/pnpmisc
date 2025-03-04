% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_create_blank.R
\name{pdf_create_blank}
\alias{pdf_create_blank}
\title{Create pdf of blank pages}
\usage{
pdf_create_blank(
  output = NULL,
  ...,
  length = 1L,
  paper = c("special", "letter", "a4"),
  orientation = c("portrait", "landscape"),
  width = 8.5,
  height = 11,
  bg = "white",
  grob = NULL
)
}
\arguments{
\item{output}{Output pdf filename.  \code{NULL} defaults to \code{tempfile(fileext = ".pdf")}.}

\item{...}{Ignored.}

\item{length}{Number of pages to create.}

\item{paper}{Paper size.  Either "letter", "a4", or "special".}

\item{orientation}{Either "portrait" or "landscape".  Ignored if \code{paper = "special"}.}

\item{width, height}{Paper size in inches if \code{paper = "special"}.}

\item{bg}{\code{output} pdf background color.}

\item{grob}{A grid grob to draw on each page
(e.g. \code{grid::textGrob("This page intentionally left blank.")}).
Default \code{NULL} is to draw nothing.}
}
\value{
\code{output} pdf file name invisibly.
As a side effect creates a blank pdf file.
}
\description{
\code{pdf_create_blank()} creates blank pdf pages.
}
\examples{
f1 <- pdf_create_blank(paper = "a4", orientation = "landscape")
f2 <- pdf_create_blank(length = 4L)
unlink(f1)
unlink(f2)
}
