% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_sat.R
\name{pr_match_GHRSST}
\alias{pr_match_GHRSST}
\title{Match locations to GHRSST}
\usage{
pr_match_GHRSST(
  df,
  pr,
  res_spat = 1,
  res_temp = "1d",
  parallel = FALSE,
  ncore = NULL
)
}
\arguments{
\item{df}{dataframe containing latitude, longitude and Date}

\item{pr}{products from list above, single or as a list}

\item{res_spat}{Number of spatial pixels to average over}

\item{res_temp}{Temporal resolution of satellite data to use}

\item{parallel}{Should the analysis run using parallel processing}

\item{ncore}{If \code{parallel = TRUE} package will use all available cores, apart from 2 which will be left for system processes and multitasking. If you wish to specify how many cores the package should use, set \code{ncore}. Otherwise, leave it as NULL.}
}
\value{
df with product output attached
}
\description{
Optional Inputs:
res_spat - Spatial resolution. How many pixels (n x n) to download in each direction
res_temp - What temporal averaging: 1 day (1d), 6 day (6d), 1 month(1m), ,...
Monthly climatology (1mNy), Annual climatology (12mNy)
Possible products to download are:
dt_analysis, l2p_flags, quality_level, satellite_zenith_angle, sea_ice_fraction, sea_ice_fraction_dtime_from_sst,
sea_surface_temperature, sses_bias, sses_count, sses_standard_deviation,
sst_count, sst_dtime, sst_mean, sst_standard_deviation, wind_speed, wind_speed_dtime_from_sst
}
\examples{
df <- tail(pr_get_DataLocs("CPR") \%>\%
        dplyr::arrange(Date), 5)
pr = c("sea_surface_temperature", "quality_level", "sst_mean", "sst_standard_deviation")
sstout <- pr_match_GHRSST(df, pr, res_spat = 10, res_temp = "6d")

}
