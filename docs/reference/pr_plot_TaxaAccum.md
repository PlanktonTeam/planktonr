# Plot taxonomic accumulation curves showing cumulative species discovery over time

Plot a taxa accumulation curve for everything that is identified by the
IMOS plankton team

## Usage

``` r
pr_plot_TaxaAccum(dat)
```

## Arguments

- dat:

  A dataframe of plankton data

## Value

a ggplot object.

## Examples

``` r
dat <- pr_get_TaxaAccum(Survey = "NRS", Type = "Zooplankton")
p <- pr_plot_TaxaAccum(dat)
dat <- pr_get_TaxaAccum(Survey = "CPR", Type = "Phytoplankton")
p <- pr_plot_TaxaAccum(dat)
```
