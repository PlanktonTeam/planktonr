# Plot Gantt Chart showing plankton sampling status

Plot Gantt Chart showing plankton sampling status

## Usage

``` r
pr_plot_Gantt(dat)
```

## Arguments

- dat:

  Trip data for either NRS or CPR.

## Value

a ggplot

## Examples

``` r
dat <- pr_get_CPRTrips()
#> Warning: `pr_get_CPRTrips()` was deprecated in planktonr 0.7.0.
#> ℹ Please use `pr_get_trips()` instead.
gg <- pr_plot_Gantt(dat)
dat <- pr_get_NRSTrips()
#> Warning: `pr_get_NRSTrips()` was deprecated in planktonr 0.7.0.
#> ℹ Please use `pr_get_trips()` instead.
gg <- pr_plot_Gantt(dat)
```
