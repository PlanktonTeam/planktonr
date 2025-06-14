% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.planktonr_dat}
\alias{print.planktonr_dat}
\title{Print method for planktonr_dat objects}
\usage{
\method{print}{planktonr_dat}(x, ..., n = NULL, width = NULL)
}
\arguments{
\item{x}{A \code{planktonr_dat} object to print.}

\item{...}{Additional arguments passed to the \code{print.tbl_df} method,
such as \code{n} for the number of rows or \code{width} for output width.}

\item{n}{<\code{\link[base:integer]{integer}}> Number of rows to print. If \code{NULL},
the default from \code{tibble} options will be used.}

\item{width}{<\code{\link[base:integer]{integer}}> Width of the output. If \code{NULL},
the default from \code{tibble} options will be used.}
}
\value{
The \code{planktonr_dat} object \code{x}, invisibly.
}
\description{
Provides a custom print method for \code{planktonr_dat} objects.
It first displays the custom attributes associated with the \code{planktonr_dat} object,
such as \code{Type}, \code{Survey}, \code{Variable}, and \code{Model}, before printing
the underlying tibble data. This provides immediate context for the data.

The \code{Model} attribute is specially handled: if it contains \code{stats::lm} objects,
a concise summary of the number of models is printed instead of the full model output.
}
