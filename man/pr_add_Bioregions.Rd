% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr_add_bioregions.R
\name{pr_add_Bioregions}
\alias{pr_add_Bioregions}
\title{Add bioregions data to existing df}
\usage{
pr_add_Bioregions(df, near_dist_km = 0)
}
\arguments{
\item{df}{A dataframe with columns \code{.data$Longitude} and \code{.data$Latitude}}

\item{near_dist_km}{What buffer distance to use to find a BioRegion match
#'}
}
\value{
A dataframe with Marine Bioregions added
}
\description{
Add bioregions data to existing df
}
\examples{
 df <- pr_get_Raw("cpr_derived_indices_data") \%>\%
  pr_rename() \%>\%
   pr_add_Bioregions()

 df <- pr_get_Raw("cpr_derived_indices_data") \%>\%
  pr_rename() \%>\%
   pr_add_Bioregions(near_dist_km = 250)

}
