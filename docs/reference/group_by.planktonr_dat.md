# Group a planktonr_dat object by one or more variables

This is an S3 method for
[`dplyr::group_by`](https://dplyr.tidyverse.org/reference/group_by.html)
that ensures `planktonr_dat` attributes are preserved and the object
correctly becomes a `grouped_df` while retaining its `planktonr_dat`
class.

## Usage

``` r
# S3 method for class 'planktonr_dat'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))
```

## Arguments

- .data:

  A `planktonr_dat` object.

- ...:

  \<[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)\>
  Variables to group by.

- .add:

  When `TRUE`, add to the existing groups, otherwise start with new
  groups.

- .drop:

  When `TRUE`, empty groups are dropped.

## Value

A `planktonr_dat` object that is also a `grouped_df`, preserving
original attributes.
