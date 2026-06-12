# Create or modify columns in a planktonr_dat object

This is an S3 method for
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)
that ensures `planktonr_dat` attributes are preserved when adding or
modifying columns.

## Usage

``` r
# S3 method for class 'planktonr_dat'
mutate(.data, ...)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Name-value pairs of expressions, and optional arguments `.before`,
  `.after`, `.keep`, `.width` to control column placement and selection.
  See
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  for details.

## Value

A `planktonr_dat` object with modified or new columns, preserving
original attributes.
