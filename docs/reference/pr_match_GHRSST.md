# Match plankton sample locations to GHRSST sea surface temperature data

Optional Inputs: res_spat - Spatial resolution. How many pixels (n x n)
to download in each direction res_temp - What temporal averaging: 1 day
(1d), 6 day (6d), 1 month(1m), ,... Monthly climatology (1mNy), Annual
climatology (12mNy) Possible products to download are: dt_analysis,
l2p_flags, quality_level, satellite_zenith_angle, sea_ice_fraction,
sea_ice_fraction_dtime_from_sst, sea_surface_temperature, sses_bias,
sses_count, sses_standard_deviation, sst_count, sst_dtime, sst_mean,
sst_standard_deviation, wind_speed, wind_speed_dtime_from_sst

## Usage

``` r
pr_match_GHRSST(
  dat,
  pr,
  res_spat = 1,
  res_temp = "1d",
  parallel = FALSE,
  ncore = NULL
)
```

## Arguments

- dat:

  dataframe containing latitude, longitude and Date

- pr:

  products from list above, single or as a list

- res_spat:

  Number of spatial pixels to average over

- res_temp:

  Temporal resolution of satellite data to use

- parallel:

  Should the analysis run using parallel processing

- ncore:

  If `parallel = TRUE` package will use all available cores, apart from
  2 which will be left for system processes and multitasking. If you
  wish to specify how many cores the package should use, set `ncore`.
  Otherwise, leave it as NULL.

## Value

dat with product output attached

## Examples

``` r
dat <- tail(pr_get_DataLocs("CPR") %>%
        dplyr::arrange(Date), 5)
pr = c("sea_surface_temperature", "quality_level", "sst_mean", "sst_standard_deviation")
sstout <- pr_match_GHRSST(dat, pr, res_spat = 10, res_temp = "6d")
#> Error in R_nc4_open: NetCDF: file not found
#> Error in R_nc4_open: NetCDF: file not found
#> Error in R_nc4_open: NetCDF: file not found
#> Error in R_nc4_open: NetCDF: file not found
#> Error in R_nc4_open: NetCDF: file not found
```
