# Load NRS chemistry data

**\[deprecated\]**

`pr_get_NRSChemistry()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
which provides a unified interface for all data types.

## Usage

``` r
pr_get_NRSChemistry()
```

## Value

A dataframe with NRS chemistry data.

## See also

[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_NRSChemistry()

# Use:
dat <- pr_get_data(Survey = "NRS", Type = "Chemistry")
} # }
```
