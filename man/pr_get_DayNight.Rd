% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plankton.R
\name{pr_get_DayNight}
\alias{pr_get_DayNight}
\title{Get data for plots of species abundance by day and night using CPR data}
\usage{
pr_get_DayNight(Type = "Zooplankton")
}
\arguments{
\item{Type}{"Phytoplankton" or "Zooplankton" (default)}
}
\value{
df to be used with pr_plot_DayNight
}
\description{
Get data for plots of species abundance by day and night using CPR data
}
\examples{
df <- pr_get_DayNight(Type = "Zooplankton")
}
