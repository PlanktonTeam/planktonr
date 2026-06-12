# Import NRS Station information

**\[deprecated\]**

`pr_get_Stations()` was deprecated in planktonr 0.7.0. Station
information is now available through
[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
with `Source = "NRS"` or `Source = "SOTS"`.

## Usage

``` r
pr_get_Stations(Survey = "NRS")
```

## Arguments

- Survey:

  Survey type: "NRS" or "SOTS"

## Value

A dataframe with station information

## See also

[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_Stations('NRS')

# Use:
dat <- pr_get_info(Source = "NRS")
} # }
```
