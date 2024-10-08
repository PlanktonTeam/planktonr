% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_maps.R
\name{pr_plot_PCImap}
\alias{pr_plot_PCImap}
\title{PCI plot for CPR data}
\usage{
pr_plot_PCImap(df)
}
\arguments{
\item{df}{dataframe with location and seasonal PCI data}
}
\value{
plot of PCI around Australia
}
\description{
PCI plot for CPR data
}
\examples{
pr_get_PCIData() \%>\% pr_plot_PCImap()
}
