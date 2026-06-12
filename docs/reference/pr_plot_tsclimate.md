# Combined timeseries and climatology plots

Combined timeseries and climatology plots

## Usage

``` r
pr_plot_tsclimate(df, trans = "identity")
```

## Arguments

- df:

  data frame with SampleDate_Local, time period and parameter

- trans:

  scale of y axis on plot, whatever scale_y_continuous trans accepts

## Value

a combined plot

## Examples

``` r
df <- pr_get_Indices(Survey = "CPR", Type = "Phytoplankton") %>%
  dplyr::filter(Parameters == "PhytoAbundance_Cellsm3")
pr_plot_tsclimate(df)
#> Warning: Removed 1335 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 25892 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 30 rows containing missing values or values outside the scale range
#> (`geom_col()`).
#> Warning: Removed 75 rows containing missing values or values outside the scale range
#> (`geom_col()`).
```
