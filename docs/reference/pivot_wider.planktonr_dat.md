# Reshape data from long to wide format for planktonr_dat objects

This is an S3 method for
[`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
that ensures `planktonr_dat` attributes are preserved when reshaping the
data.

## Usage

``` r
# S3 method for class 'planktonr_dat'
pivot_wider(data, ...)
```

## Arguments

- data:

  A `planktonr_dat` object.

- ...:

  Arguments passed on to
  [`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## Value

A `planktonr_dat` object with data reshaped to wide format, preserving
original attributes.
