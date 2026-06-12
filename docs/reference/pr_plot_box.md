# Simple boxplot function using common NRS colouring

Note that this function assumes wide data with the data to plot as
columns.

## Usage

``` r
pr_plot_box(dat, y)
```

## Arguments

- dat:

  Dataframe

- y:

  Column name for the y axis

## Value

ggplot object

## Examples

``` r
dat <- pr_get_data(Survey = "Coastal", Type = "Micro") %>%
tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
dplyr::filter(StationCode %in% c("TOP", "BAI")) %>%
tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean)
gg <- pr_plot_box(dat, "Bacterial_Temperature_Index_KD")
```
