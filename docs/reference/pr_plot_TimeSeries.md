# Plot basic timeseries of plankton indices

Create a time series plot showing how plankton indices vary over time at
different stations or bioregions. This is a simple line plot without
trend analysis or aggregation.

## Usage

``` r
pr_plot_TimeSeries(df, trans = "identity")
```

## Arguments

- df:

  A dataframe from
  [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
  containing timeseries data

- trans:

  Transformation for the y-axis scale. Options include:

  - `"identity"` - No transformation (default)

  - `"log10"` - Log base 10 transformation (useful for abundance data)

  - `"sqrt"` - Square root transformation

  - `"log"` - Natural log transformation

  - Any other transformation accepted by
    [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

## Value

A ggplot2 object showing the timeseries

## Details

For NRS data, values are averaged across depths if multiple depths are
present (relevant for microbial data). For CPR data, bioregions are
treated as "stations" for plotting purposes.

The plot uses colour and line type to distinguish between
stations/bioregions. Points show individual observations, connected by
lines.

## See also

[`pr_plot_Trends()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Trends.md)
for timeseries with trend lines,
[`pr_plot_Climatology()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Climatology.md)
for seasonal patterns

## Examples

``` r
# Plot NRS zooplankton biomass
df <- pr_get_Indices("NRS", "Zooplankton") %>%
  dplyr::filter(Parameters == "Biomass_mgm3", StationCode %in% c("NSI", "PHB"))
pr_plot_TimeSeries(df)
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 15 rows containing missing values or values outside the scale range
#> (`geom_point()`).


# Use log scale for abundance data
df <- pr_get_Indices("NRS", "Phytoplankton") %>%
  dplyr::filter(Parameters == "PhytoAbundance_CellsL", StationCode %in% c("MAI", "PHB"))
pr_plot_TimeSeries(df, trans = "log10")
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 22 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```
