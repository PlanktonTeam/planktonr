# Remove statistical outliers from plankton data

Identify and remove outliers using a threshold based on standard
deviations from the mean. Outliers are determined separately for each
combination of parameter, location (station or bioregion), and depth.

## Usage

``` r
pr_remove_outliers(df, x)
```

## Arguments

- df:

  A dataframe from
  [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md),
  [`pr_get_EOVs()`](https://planktonteam.github.io/planktonr/reference/pr_get_EOVs.md),
  or similar functions containing plankton data

- x:

  Number of standard deviations from the mean to use as the threshold.
  Typical values are:

  - `2` - Removes extreme outliers (~5% of data if normally distributed)

  - `3` - More conservative, removes only very extreme values (~0.3%)

  - `1.5` - More aggressive, removes more data

## Value

A dataframe with outliers removed, maintaining the same structure as
input

## Details

The function calculates outlier thresholds for each combination of:

- Parameter (e.g., biomass, diversity)

- Location (station code or bioregion)

- Depth (if present in data)

Values outside the range `mean ± (x × SD)` are removed. This approach:

- Preserves natural variability while removing measurement errors

- Handles each parameter-location-depth combination independently

- Automatically converts negative values to zero (biological data should
  be non-negative)

### When to Use

Use outlier removal when:

- Data contain obvious measurement or transcription errors

- Preparing data for statistical modelling

- Creating publication figures

### Caution

- Real extreme events (e.g., blooms, upwelling) may be removed

- Always inspect data before and after removal

- Consider whether removed values represent true outliers or important
  biological events

- Document outlier removal in methods sections

Typically use `x = 2` as a reasonable balance between removing errors
and preserving real variability.

## See also

[`pr_model_data()`](https://planktonteam.github.io/planktonr/reference/pr_model_data.md)
which typically follows outlier removal for trend analysis

## Examples

``` r
# Remove outliers using 2 SD threshold (recommended)
df <- pr_get_Indices("NRS", "Zooplankton") %>%
  pr_remove_outliers(2)

# More conservative removal (3 SD)
df <- pr_get_Indices("CPR", "Phytoplankton") %>%
  pr_remove_outliers(3)

# Check how many values were removed
df_before <- pr_get_EOVs("NRS") %>%
  dplyr::filter(Parameters == "Biomass_mgm3")
df_after <- df_before %>% pr_remove_outliers(2)
nrow(df_before) - nrow(df_after)
#> [1] 117
```
