# Match plankton samples to satellite altimetry data (sea level, currents)

Match plankton samples to satellite altimetry data (sea level, currents)

## Usage

``` r
pr_match_Altimetry(dat, pr, res_spat = 1)
```

## Arguments

- dat:

  dataframe containing Latitude, Longitude and Date

- pr:

  products from GSLA, GSL, UCUR, UCUR, VCUR, UCUR_MEAN, VCUR_MEAN,
  single or as a list

- res_spat:

  Number of spatial pixels to average over

## Value

dat with product output attached

## Examples

``` r
dat <- pr_get_DataLocs("NRS") %>%
  dplyr::filter(Date < lubridate::ymd("2019-12-31")) %>%
  head(5)
altout <- pr_match_Altimetry(dat, pr = "GSLA", res_spat = 10)
```
