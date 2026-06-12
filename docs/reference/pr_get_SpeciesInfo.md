# Get species information table for Phytoplankton and Zooplankton

**\[deprecated\]**

`pr_get_SpeciesInfo()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
which provides a unified interface.

## Usage

``` r
pr_get_SpeciesInfo(Type = "Zooplankton")
```

## Arguments

- Type:

  Phytoplankton (P) or Zooplankton (Z)

## Value

A dataframe of species information

## See also

[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_SpeciesInfo(Type = "Phytoplankton")

# Use:
dat <- pr_get_info(Source = "Phytoplankton")
} # }
```
