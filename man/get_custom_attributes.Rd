% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verbs.R
\name{get_custom_attributes}
\alias{get_custom_attributes}
\title{Internal helper to extract custom attributes from a planktonr_dat object}
\usage{
get_custom_attributes(.data)
}
\arguments{
\item{.data}{A \code{planktonr_dat} object.}
}
\value{
A named list of custom attributes.
}
\description{
This function identifies and collects all attributes attached to a
\code{planktonr_dat} object that are not standard R, tibble, or dplyr attributes.
This allows these custom attributes to be preserved across data manipulation
operations.
}
\keyword{internal}
