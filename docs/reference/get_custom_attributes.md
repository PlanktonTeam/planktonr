# Internal helper to extract custom attributes from a planktonr_dat object

This function identifies and collects all attributes attached to a
`planktonr_dat` object that are not standard R, tibble, or dplyr
attributes. This allows these custom attributes to be preserved across
data manipulation operations.

## Usage

``` r
get_custom_attributes(.data)
```

## Arguments

- .data:

  A `planktonr_dat` object.

## Value

A named list of custom attributes.
