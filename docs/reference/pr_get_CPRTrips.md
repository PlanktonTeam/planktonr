# Get CPR sampling trip metadata

**\[deprecated\]**

`pr_get_CPRTrips()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
which provides a unified interface for both NRS and CPR trip metadata.

## Usage

``` r
pr_get_CPRTrips(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md),
  which passes them to
  [`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md).
  Currently supports:

  - `near_dist_km` - Distance in kilometres to pad bioregion boundaries

## Value

A dataframe with CPR trip information.

## See also

[`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_CPRTrips()

# Use:
dat <- pr_get_trips(Survey = "CPR")

# With bioregion padding:
dat <- pr_get_trips(Survey = "CPR", near_dist_km = 250)
} # }
```
