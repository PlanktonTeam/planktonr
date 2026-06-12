# Plot environmental contours showing depth-time patterns

Create contour plots showing how environmental variables change with
both time (x-axis) and depth (y-axis). This visualisation is
particularly effective for identifying vertical structure,
stratification, and the deep chlorophyll maximum.

## Usage

``` r
pr_plot_NRSEnvContour(dat, na.fill = TRUE)
```

## Arguments

- dat:

  A dataframe from
  [`pr_get_NRSEnvContour()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSEnvContour.md)
  containing environmental data formatted for contour plotting

- na.fill:

  How to handle missing data (gaps) in the contour:

  - `TRUE` - Fill gaps using linear interpolation (default, creates
    smoother contours)

  - `FALSE` - Leave gaps as-is (shows only measured data)

  - A function - Use custom interpolation (e.g., `mean`, `median`)

## Value

A ggplot2 object with contour plots faceted by station and parameter

## Details

Contour plots are excellent for visualising:

### Vertical Structure

- Deep chlorophyll maximum (DCM) depth and intensity

- Nutricline depth and strength

- Stratification patterns (steep vs. gradual gradients)

- Surface vs. subsurface maxima

### Temporal Patterns

- Seasonal shoaling/deepening of features

- Upwelling events (nutrient-rich water at surface)

- Mixing events (homogenisation of the water column)

- Long-term changes in vertical structure

### Gap Filling

When `na.fill = TRUE`, the function uses
[`metR::geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.html)
with linear interpolation to create smooth contours. This is appropriate
for environmental data with regular sampling patterns and small gaps.
For irregular sampling or large gaps, consider setting `na.fill = FALSE`
to show only measured data.

The function automatically creates one facet per station and parameter
combination. Contours are filled with a colour scale, and contour lines
are overlaid in grey.

## See also

[`pr_get_NRSEnvContour()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSEnvContour.md)
for preparing the input data,
[`pr_plot_Enviro()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Enviro.md)
for alternative line plot visualisation,
[`pr_get_NRSChemistry()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSChemistry.md),
[`pr_get_NRSPigments()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSPigments.md),
[`pr_get_NRSPico()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSPico.md)
for data sources

## Examples

``` r
dat <- pr_get_NRSEnvContour("Chemistry") %>% dplyr::filter(Parameters == "Nitrate_umolL",
StationCode %in% c('YON', 'MAI', 'PHB', 'NSI'))
plot <- pr_plot_NRSEnvContour(dat, na.fill = TRUE)
```
