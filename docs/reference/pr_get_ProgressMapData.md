# Prepare sampling coverage data for IMOS progress map visualisation

Prepare sampling coverage data for IMOS progress map visualisation

## Usage

``` r
pr_get_ProgressMapData(Survey = c("NRS", "CPR"), interactive = FALSE, ...)
```

## Arguments

- Survey:

  one of NRS, CPR or Both

- interactive:

  A logical TRUE/FALSE if the data is to be used for an interactive
  plot.

- ...:

  variables to be passed to pr_add_Bioregions. At the moment it only
  supports `near_dist_km` which is the distance (km) around each
  bioregion to pad the allocation of points.

## Value

A dataframe for input into pr_plot_Progress()

## Examples

``` r
dat <- pr_get_ProgressMapData(c("NRS", "CPR"))
dat <- pr_get_ProgressMapData(c("NRS", "CPR"), interactive = TRUE)
```
