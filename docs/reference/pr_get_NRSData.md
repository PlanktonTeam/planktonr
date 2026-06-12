# Import NRS plankton data

**\[deprecated\]**

`pr_get_NRSData()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
which provides a unified interface for all data types.

## Usage

``` r
pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
```

## Arguments

- Type:

  The data of interest: `"Phytoplankton"` or `"Zooplankton"`

- Variable:

  Variable: `"abundance"` or `"biovolume"`

- Subset:

  Data level: `"raw"`, `"htg"`, `"genus"`, `"species"`, or `"copepods"`

## Value

A dataframe with NRS plankton data.

## See also

[`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")

# Use:
dat <- pr_get_data(Survey = "NRS", Type = "Phytoplankton",
                   Variable = "abundance", Subset = "raw")
} # }
```
