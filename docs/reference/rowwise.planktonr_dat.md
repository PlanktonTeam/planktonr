# Convert a planktonr_dat object to a rowwise data frame

This is an S3 method for
[`dplyr::rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)
that ensures `planktonr_dat` attributes are preserved and the object
correctly becomes a `rowwise_df` while retaining its `planktonr_dat`
class.

## Usage

``` r
# S3 method for class 'planktonr_dat'
rowwise(data, ..., .rows = NULL)
```

## Arguments

- data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Columns to group by.
  [`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)
  groups by all columns by default.

- .rows:

  Not currently used by
  [`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html),
  but included for compatibility with future `dplyr` updates.

## Value

A `planktonr_dat` object that is also a `rowwise_df`, preserving
original attributes.
