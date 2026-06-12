# Fit linear models to plankton time series data

Fit linear models to time series data for trend analysis, accounting for
both long-term trends and seasonal cycles. This function is typically
used before extracting coefficients or plotting trends with model
predictions.

## Usage

``` r
pr_model_data(dat)
```

## Arguments

- dat:

  A dataframe from
  [`pr_get_EOVs()`](https://planktonteam.github.io/planktonr/reference/pr_get_EOVs.md),
  [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md),
  or similar functions containing time series data. Must contain only
  one parameter at a time.

## Value

The input dataframe with model objects stored as a "Model" attribute

## Details

The function fits linear models of the form:

`Values ~ Year + sin(Month) + cos(Month)`

Where:

- **Year term** captures long-term linear trends (increasing or
  decreasing over time)

- **Harmonic terms** capture seasonal cycles using Fourier basis
  functions (k=1, allowing for one complete seasonal cycle per year)

The model is fitted separately for each station (NRS/LTM) or bioregion
(CPR). For LTM data, values are first averaged across depths to create a
single depth- integrated estimate.

### Model Interpretation

- **Positive Year coefficient**: Variable increasing over time

- **Negative Year coefficient**: Variable decreasing over time

- **Significant harmonic terms**: Strong seasonal cycle present

The fitted model objects are stored as an attribute of the dataframe and
can be extracted using
[`pr_get_model()`](https://planktonteam.github.io/planktonr/reference/pr_get_model.md)
and analysed using
[`pr_get_coeffs()`](https://planktonteam.github.io/planktonr/reference/pr_get_coeffs.md).

### Limitations

- Assumes linear trends (non-linear trends may be better captured with
  GAMs)

- Assumes single seasonal cycle (some variables may have sub-annual
  variation)

- Only handles one parameter at a time

## See also

[`pr_get_model()`](https://planktonteam.github.io/planktonr/reference/pr_get_model.md)
to extract model objects,
[`pr_get_coeffs()`](https://planktonteam.github.io/planktonr/reference/pr_get_coeffs.md)
to extract tidy coefficients,
[`pr_remove_outliers()`](https://planktonteam.github.io/planktonr/reference/pr_remove_outliers.md)
to remove outliers before modelling

## Examples

``` r
# Fit models to zooplankton biomass
dat <- pr_get_EOVs(Survey = "NRS") %>%
  dplyr::filter(Parameters == "Biomass_mgm3") %>%
  pr_remove_outliers(2) %>%
  pr_model_data()

# Extract and view model coefficients
models <- pr_get_model(dat)
coeffs <- pr_get_coeffs(models)
print(coeffs)
#> # A tibble: 40 × 7
#>    Station         term              estimate std.error statistic p.value signif
#>    <chr>           <chr>                <dbl>     <dbl>     <dbl>   <dbl> <chr> 
#>  1 Darwin          (Intercept)       -4.86e+3  2354.       -2.07  4.08e-2 "*"   
#>  2 Darwin          Year_Local         2.44e+0     1.17      2.09  3.85e-2 "*"   
#>  3 Darwin          pr_harmonic(Mont… -5.66e+0     6.52     -0.868 3.87e-1 ""    
#>  4 Darwin          pr_harmonic(Mont… -3.23e+0     7.11     -0.454 6.51e-1 ""    
#>  5 Esperance       (Intercept)        1.65e+2  1177.        0.140 8.91e-1 ""    
#>  6 Esperance       Year_Local        -7.79e-2     0.585    -0.133 8.97e-1 ""    
#>  7 Esperance       pr_harmonic(Mont… -1.31e+0     1.01     -1.29  2.24e-1 ""    
#>  8 Esperance       pr_harmonic(Mont…  2.87e+0     1.18      2.44  3.29e-2 "*"   
#>  9 Kangaroo Island (Intercept)       -1.12e+3   255.       -4.39  4.62e-5 "***" 
#> 10 Kangaroo Island Year_Local         5.59e-1     0.127     4.42  4.21e-5 "***" 
#> # ℹ 30 more rows

# Fit models to CPR phytoplankton diversity
dat_cpr <- pr_get_EOVs(Survey = "CPR") %>%
  dplyr::filter(Parameters == "ShannonPhytoDiversity") %>%
  pr_remove_outliers(2) %>%
  pr_model_data()
```
