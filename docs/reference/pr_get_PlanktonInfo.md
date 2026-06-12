# Load taxonomic information and trait data for plankton species

**\[deprecated\]**

`pr_get_PlanktonInfo()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
which provides a unified interface for both plankton taxonomic
information and survey policy information.

## Usage

``` r
pr_get_PlanktonInfo(Type = "Zooplankton")
```

## Arguments

- Type:

  The type of plankton: "Phytoplankton" or "Zooplankton" (also accepts
  "P", "Z", "Phyto", "Zoop")

## Value

A dataframe with taxonomic and trait information.

## See also

[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
zoo_info <- pr_get_PlanktonInfo(Type = "Zooplankton")

# Use:
zoo_info <- pr_get_info(Source = "Zooplankton")
} # }
```
