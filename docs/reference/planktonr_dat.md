# Define the constructor function for the planktonr data class

This function creates a `planktonr_dat` object, which is a tibble with
additional custom attributes for metadata. It can accept either a
`data.frame` or a `tibble`, coercing `data.frame` inputs to `tibble`.

## Usage

``` r
planktonr_dat(
  .data,
  Type = NULL,
  Survey = NULL,
  Variable = NULL,
  Model = NULL,
  ...
)
```

## Arguments

- .data:

  The data `data.frame` or `tibble` to be converted to the
  `planktonr_dat` class.

- Type:

  The data type. Must be one of "Microbes", "Phytoplankton",
  "Zooplankton", "Water", or "EOV".

- Survey:

  The survey of the data. E.g., "NRS", "CPR", "LTM", "GO-SHIP",
  "Coastal", "HAB".

- Variable:

  What variable is being described or subsetted by this data.

- Model:

  Optional model associated with the data.

- ...:

  Additional attributes to be stored with the `planktonr_dat` object.
  These will be stored as attributes on the object.

## Value

An object of class `planktonr_dat`.

## Examples

``` r
# Create from a tibble (existing behavior)
tibble_data <- tibble::tibble(
  time = as.Date(c("2023-01-01", "2023-01-02")),
  value = c(10, 20)
)
pr_obj_tibble <- planktonr_dat(
  .data = tibble_data,
  Type = "Phytoplankton",
  Survey = "NRS",
  Variable = "Chlorophyll_a"
)
print(pr_obj_tibble)
#> --- planktonr_dat Attributes ---
#>   Type: Phytoplankton
#>   Survey: NRS
#>   Variable: Chlorophyll_a
#> 
#> # A tibble: 2 × 2
#>   time       value
#>   <date>     <dbl>
#> 1 2023-01-01    10
#> 2 2023-01-02    20

# Create from a base R data.frame (newly supported)
df_data <- data.frame(
  time = as.Date(c("2023-01-03", "2023-01-04")),
  value = c(30, 40)
)
pr_obj_df <- planktonr_dat(
  .data = df_data,
  Type = "Zooplankton",
  Survey = "CPR",
  Variable = "Biomass"
)
print(pr_obj_df)
#> --- planktonr_dat Attributes ---
#>   Type: Zooplankton
#>   Survey: CPR
#>   Variable: Biomass
#> 
#> # A tibble: 2 × 2
#>   time       value
#>   <date>     <dbl>
#> 1 2023-01-03    30
#> 2 2023-01-04    40
class(pr_obj_df) # Still a planktonr_dat (which inherits from tibble)
#> [1] "planktonr_dat" "tbl_df"        "tbl"           "data.frame"   
```
