# Create map showing CPR voyage tracks and sampling locations

Create map showing CPR voyage tracks and sampling locations

## Usage

``` r
pr_plot_Voyagemap(dat, dat_select, Country = c("Australia"))
```

## Arguments

- dat:

  dataframe containing all locations to plot

- dat_select:

  dataframe of sample locations to highlight

- Country:

  countries to plot on map

## Value

a map of the selected bioregions

## Examples

``` r
dat <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")
dat_select <- dat %>% dplyr::slice(1:5000)
voyagemap <- pr_plot_Voyagemap(dat, dat_select, Country = c("Australia", "New Zealand"))
```
