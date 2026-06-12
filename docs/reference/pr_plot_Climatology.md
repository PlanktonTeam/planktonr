# Plot climatologies showing seasonal or interannual patterns

Create bar plots showing monthly or annual climatologies (mean values
with error bars) to examine seasonal cycles or interannual variability
in plankton indices. Data are aggregated across years (monthly) or
within years (annual) to show typical patterns.

## Usage

``` r
pr_plot_Climatology(df, Trend = "Month", trans = "identity")
```

## Arguments

- df:

  A dataframe from
  [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
  containing timeseries data

- Trend:

  The temporal aggregation for climatology:

  - `"Month"` - Monthly climatology averaged across all years (shows
    seasonal patterns)

  - `"Year"` - Annual means for each year (shows interannual
    variability)

- trans:

  Transformation for the y-axis scale:

  - `"identity"` - No transformation (default)

  - `"log10"` - Log base 10 transformation (useful for abundance data)

  - `"sqrt"` - Square root transformation

  - Any other transformation accepted by
    [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

## Value

A ggplot2 bar plot with error bars showing climatology patterns

## Details

### Monthly Climatology (Trend = "Month")

Calculates the mean value for each month across all years in the
dataset. This shows the typical seasonal cycle, useful for identifying:

- Spring phytoplankton blooms

- Summer stratification effects

- Winter mixing impacts

- Seasonal migration patterns in zooplankton

### Annual Climatology (Trend = "Year")

Calculates the mean value for each calendar year. This shows
year-to-year variability, useful for identifying:

- Long-term trends (increasing or decreasing)

- Regime shifts

- Responses to climate oscillations (e.g., ENSO, SAM)

- Extreme years (e.g., marine heatwaves)

Error bars represent standard error of the mean (±SE), calculated as
SD/√N where N is the number of observations.

For NRS data, values are averaged across depths if present.

## See also

[`pr_plot_Trends()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Trends.md)
for alternative climatology visualisation with trend lines,
[`pr_plot_TimeSeries()`](https://planktonteam.github.io/planktonr/reference/pr_plot_TimeSeries.md)
for raw time series plots

## Examples

``` r
df <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
dplyr::filter(Parameters == "PhytoBiomassCarbon_pgL", StationCode %in% c("NSI", "PHB"))

monthly <- pr_plot_Climatology(df, Trend = "Month")

df <- pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
        dplyr::filter(Parameters == "ZoopAbundance_m3")
annual <- pr_plot_Climatology(df, Trend = "Year")
```
