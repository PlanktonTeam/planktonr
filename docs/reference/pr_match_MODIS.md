# Match plankton samples to MODIS ocean colour data (chlorophyll, PAR, SST)

Match plankton samples to MODIS ocean colour data (chlorophyll, PAR,
SST)

## Usage

``` r
pr_match_MODIS(dat, pr, res_spat = 1, res_temp = "1d")
```

## Arguments

- dat:

  dataframe containing latitude, longitude and Date

- pr:

  products from K_490, chl_carder, chl_gsm, chl_oc3, chl_oci, dt, ipar,
  l2_flags, owtd, par, sst, sst_quality single or as a list

- res_spat:

  Number of spatial pixels to average over

- res_temp:

  Temporal resolution of satellite data to use

## Value

dat with product output attached

## Examples

``` r
dat <- head(pr_get_DataLocs("NRS"),5)
MODISout <- pr_match_MODIS(dat, pr <- c("chl_gsm", "chl_oc3"), res_spat = 10)
```
