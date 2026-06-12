# Extract tidy model coefficients for trend analysis

Extract and format model coefficients from fitted linear models,
including significance levels and standard errors. This is useful for
creating tables of trend statistics or identifying significant trends
across multiple stations.

## Usage

``` r
pr_get_coeffs(Models, id = "Station")
```

## Arguments

- Models:

  A list of model objects extracted using
  [`pr_get_model()`](https://planktonteam.github.io/planktonr/reference/pr_get_model.md)
  from a dataframe that has been processed with
  [`pr_model_data()`](https://planktonteam.github.io/planktonr/reference/pr_model_data.md)

- id:

  Name for the column containing model identifiers (station names or
  bioregion names). Default is `"Station"`.

## Value

A tibble containing model coefficients with columns:

- Model identifier column (name specified by `id` parameter)

- `term` - Model term name

- `estimate` - Coefficient estimate

- `std.error` - Standard error of estimate

- `statistic` - t-statistic

- `p.value` - P-value

- `signif` - Significance indicator (\*, \*\*, \*\*\*)

## Details

The function uses
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) to
extract coefficients from each model and adds significance indicators:

- `***` p ≤ 0.001 (highly significant)

- `**` p ≤ 0.01 (very significant)

- `*` p ≤ 0.05 (significant)

- (blank) p \> 0.05 (not significant)

For NRS/LTM data, station codes are automatically added if station names
are used as identifiers.

### Interpreting Coefficients

- **Year_Local**: The rate of change per year (slope). Positive =
  increasing, negative = decreasing

- **pr_harmonic(Month, k = 1)1**: Sine component of seasonal cycle

- **pr_harmonic(Month, k = 1)2**: Cosine component of seasonal cycle

- **Intercept**: Expected value at Year = 0 (often not interpretable)

The Year_Local coefficient is typically of most interest for detecting
long-term trends. Significant harmonic terms indicate a strong seasonal
cycle.

## See also

[`pr_model_data()`](https://planktonteam.github.io/planktonr/reference/pr_model_data.md)
for fitting models,
[`pr_get_model()`](https://planktonteam.github.io/planktonr/reference/pr_get_model.md)
for extracting model objects

## Examples

``` r
# Fit models and extract coefficients
dat <- planktonr::pr_get_EOVs(Survey = "NRS") %>%
  dplyr::filter(Parameters == "Biomass_mgm3") %>%
  pr_remove_outliers(2) %>%
  pr_model_data()

models <- pr_get_model(dat)
coeffs <- pr_get_coeffs(models)

# View only Year trends
coeffs %>% dplyr::filter(term == "Year_Local")
#> # A tibble: 10 × 7
#>    Station                 term      estimate std.error statistic p.value signif
#>    <chr>                   <chr>        <dbl>     <dbl>     <dbl>   <dbl> <chr> 
#>  1 Darwin                  Year_Loc…  2.44       1.17      2.09   3.85e-2 "*"   
#>  2 Esperance               Year_Loc… -0.0779     0.585    -0.133  8.97e-1 ""    
#>  3 Kangaroo Island         Year_Loc…  0.559      0.127     4.42   4.21e-5 "***" 
#>  4 Maria Island            Year_Loc…  0.00787    0.130     0.0604 9.52e-1 ""    
#>  5 Ningaloo                Year_Loc…  2.56       4.13      0.620  5.55e-1 ""    
#>  6 North Stradbroke Island Year_Loc…  0.0421     0.0864    0.487  6.27e-1 ""    
#>  7 Port Hacking            Year_Loc… -0.204      0.132    -1.54   1.25e-1 ""    
#>  8 Rottnest Island         Year_Loc…  0.186      0.0923    2.02   4.55e-2 "*"   
#>  9 Bonney Coast            Year_Loc… -3.05       4.46     -0.685  5.42e-1 ""    
#> 10 Yongala                 Year_Loc…  0.290      0.181     1.61   1.10e-1 ""    

# Identify stations with significant trends
coeffs %>% 
  dplyr::filter(term == "Year_Local", signif != "")
#> # A tibble: 3 × 7
#>   Station         term       estimate std.error statistic   p.value signif
#>   <chr>           <chr>         <dbl>     <dbl>     <dbl>     <dbl> <chr> 
#> 1 Darwin          Year_Local    2.44     1.17        2.09 0.0385    *     
#> 2 Kangaroo Island Year_Local    0.559    0.127       4.42 0.0000421 ***   
#> 3 Rottnest Island Year_Local    0.186    0.0923      2.02 0.0455    *     
```
