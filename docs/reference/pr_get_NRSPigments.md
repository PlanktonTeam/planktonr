# Load NRS pigments data

**\[deprecated\]**

`pr_get_NRSPigments()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
which provides a unified interface for all data types.

## Usage

``` r
pr_get_NRSPigments(Format = "all")
```

## Arguments

- Format:

  Output format: `"all"` or `"binned"`

## Value

A dataframe with NRS pigments data.

## See also

[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_NRSPigments(Format = "binned")

# Use:
dat <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")
} # }
```
