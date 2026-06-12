# Drop rows containing missing values for planktonr_dat objects

This is an S3 method for
[`tidyr::drop_na`](https://tidyr.tidyverse.org/reference/drop_na.html)
that ensures `planktonr_dat` attributes are preserved.

## Usage

``` r
# S3 method for class 'planktonr_dat'
drop_na(data, ...)
```

## Arguments

- data:

  A `planktonr_dat` object.

- ...:

  Arguments passed on to
  [`tidyr::drop_na`](https://tidyr.tidyverse.org/reference/drop_na.html).

## Value

A `planktonr_dat` object with rows containing `NA` values dropped,
preserving original attributes.
