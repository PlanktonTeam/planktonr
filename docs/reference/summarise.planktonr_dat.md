# Summarise data in a planktonr_dat object

This is an S3 method for
[`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
that ensures `planktonr_dat` attributes are preserved during
summarisation.

## Usage

``` r
# S3 method for class 'planktonr_dat'
summarise(.data, ...)
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Name-value pairs of summary statistics, and optional arguments `.by`
  and `.groups`. See
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  for details.

## Value

A `planktonr_dat` object with summarised data, preserving original
attributes. The grouping structure depends on `.groups`.
