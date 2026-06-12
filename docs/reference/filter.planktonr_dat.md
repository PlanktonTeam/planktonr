# Filter rows of a planktonr_dat object

This is an S3 method for
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
that ensures `planktonr_dat` attributes are preserved when filtering
rows.

## Usage

``` r
# S3 method for class 'planktonr_dat'
filter(.data, ...)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Logical predicates to filter by.

## Value

A `planktonr_dat` object with filtered rows, preserving original
attributes.
