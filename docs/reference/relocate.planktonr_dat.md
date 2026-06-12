# Relocate columns in a planktonr_dat object

This is an S3 method for
[`dplyr::relocate`](https://dplyr.tidyverse.org/reference/relocate.html)
that ensures `planktonr_dat` attributes are preserved when moving
columns.

## Usage

``` r
# S3 method for class 'planktonr_dat'
relocate(.data, ...)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to relocate, and optional arguments `.before` and `.after` to
  control placement. See
  [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  for details.

## Value

A `planktonr_dat` object with columns reordered, preserving original
attributes.
