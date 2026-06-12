# Get data for STI plots of species abundance

Get data for STI plots of species abundance

## Usage

``` r
pr_get_STIdata(Type = "Phytoplankton")
```

## Arguments

- Type:

  Phyto or zoo, defaults to phyto

## Value

df to be sued with pr_plot_STI

## Examples

``` r
df <- pr_get_STIdata("Phytoplankton")
df <- pr_get_STIdata("Zooplankton")
```
