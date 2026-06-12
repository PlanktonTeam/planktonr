# Load microbial data

**\[deprecated\]**

`pr_get_NRSMicro()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
which provides a unified interface for all data types.

## Usage

``` r
pr_get_NRSMicro(Survey = "NRS")
```

## Arguments

- Survey:

  Survey: `"NRS"`, `"Coastal"`, or `"GO-SHIP"`

## Value

A dataframe with microbial data.

## See also

[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_NRSMicro(Survey = "GO-SHIP")

# Use:
dat <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")
} # }
```
