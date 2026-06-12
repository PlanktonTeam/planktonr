# Create interactive map showing IMOS plankton sampling coverage and progress

Create interactive map showing IMOS plankton sampling coverage and
progress

## Usage

``` r
pr_plot_ProgressMap(dat, interactive = FALSE, labels = TRUE)
```

## Arguments

- dat:

  output from pr_get_ProgressMapData

- interactive:

  Should the plot be interactive with leaflet?

- labels:

  TRUE/FALSE Should labels be added to leaflet plot? Adding labels adds
  more information but slows down the rendering.

## Value

a plot of IMOS progress

## Examples

``` r
dat <- pr_get_ProgressMapData("NRS")
plot <- pr_plot_ProgressMap(dat)
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
```
