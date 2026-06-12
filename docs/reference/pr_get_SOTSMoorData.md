# Get data available from SOTS moorings (limited to use in BOO atm, other variables could be added, select from pr_get_SOTSvariables)

Get data available from SOTS moorings (limited to use in BOO atm, other
variables could be added, select from pr_get_SOTSvariables)

## Usage

``` r
pr_get_SOTSMoorData(Type = "Physical")
```

## Arguments

- Type:

  Nutrients or Physical

## Value

SOTS nutrients or physical parameters data

## Examples

``` r
dat <- pr_get_SOTSMoorData(Type = "Nutrients")
```
