# Arrange rows of a planktonr_dat object

This is an S3 method for
[`dplyr::arrange`](https://dplyr.tidyverse.org/reference/arrange.html)
that ensures `planktonr_dat` attributes are preserved when arranging
rows.

## Usage

``` r
# S3 method for class 'planktonr_dat'
arrange(.data, ..., .by_group = FALSE)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Columns to arrange by.

- .by_group:

  If `TRUE`, will arrange first by the grouping variables. See
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  for details.

## Value

A `planktonr_dat` object with arranged rows, preserving original
attributes.
