# Reshape data from wide to long format for planktonr_dat objects

This is an S3 method for
[`tidyr::pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
that ensures `planktonr_dat` attributes are preserved when reshaping the
data.

## Usage

``` r
# S3 method for class 'planktonr_dat'
pivot_longer(data, ...)
```

## Arguments

- data:

  A `planktonr_dat` object.

- ...:

  Arguments passed on to
  [`tidyr::pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html).

## Value

A `planktonr_dat` object with data reshaped to long format, preserving
original attributes.
