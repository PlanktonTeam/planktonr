# Load satellite-derived environmental data matched to plankton sampling locations

Load satellite-derived environmental data matched to plankton sampling
locations

## Usage

``` r
pr_get_SatData(Survey = "NRS")
```

## Arguments

- Survey:

  either NRS or CPR

## Value

dat with either NRS or CPR satellite data

## Examples

``` r
dat <- pr_get_SatData("CPR")
```
