# Import NRS sampling trip metadata

**\[deprecated\]**

`pr_get_NRSTrips()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
which provides a unified interface for both NRS and CPR trip metadata.

## Usage

``` r
pr_get_NRSTrips()
```

## Value

A dataframe with NRS trip information.

## See also

[`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_NRSTrips()

# Use:
dat <- pr_get_trips(Survey = "NRS")
} # }
```
