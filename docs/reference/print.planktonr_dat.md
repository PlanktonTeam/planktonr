# Print method for planktonr_dat objects

Provides a custom print method for `planktonr_dat` objects. It first
displays the custom attributes associated with the `planktonr_dat`
object, such as `Type`, `Survey`, `Variable`, and `Model`, before
printing the underlying tibble data. This provides immediate context for
the data.

The `Model` attribute is specially handled: if it contains
[`stats::lm`](https://rdrr.io/r/stats/lm.html) objects, a concise
summary of the number of models is printed instead of the full model
output.

## Usage

``` r
# S3 method for class 'planktonr_dat'
print(x, ..., n = NULL, width = NULL)
```

## Arguments

- x:

  A `planktonr_dat` object to print.

- ...:

  Additional arguments passed to the `print.tbl_df` method, such as `n`
  for the number of rows or `width` for output width.

- n:

  \<[`integer`](https://rdrr.io/r/base/integer.html)\> Number of rows to
  print. If `NULL`, the default from `tibble` options will be used.

- width:

  \<[`integer`](https://rdrr.io/r/base/integer.html)\> Width of the
  output. If `NULL`, the default from `tibble` options will be used.

## Value

The `planktonr_dat` object `x`, invisibly.
