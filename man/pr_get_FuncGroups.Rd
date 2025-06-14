% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plankton.R
\name{pr_get_FuncGroups}
\alias{pr_get_FuncGroups}
\title{Get functional group data}
\usage{
pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton", ...)
}
\arguments{
\item{Survey}{CPR or NRS data}

\item{Type}{Zooplankton or phytoplankton data}

\item{...}{to allow use of join when used within another function}
}
\value{
dataframe for plotting functional group time series info
}
\description{
Get functional group data
}
\examples{
NRSfgz <- pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton")
NRSfgp <- pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton")
CPRfgz <- pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250)
CPRfgp <- pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton")
}
