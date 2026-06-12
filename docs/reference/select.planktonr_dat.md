# Select, rename, or reorder columns in a planktonr_dat object

This is an S3 method for
[`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html)
that ensures `planktonr_dat` attributes are preserved when manipulating
columns.

## Usage

``` r
# S3 method for class 'planktonr_dat'
select(.data, ...)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to select, rename, or reorder.

## Value

A `planktonr_dat` object with selected columns, preserving original
attributes.
