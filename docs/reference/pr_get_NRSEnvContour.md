# Prepare biogeochemical data for depth-time contour plots

Prepare biogeochemical data for depth-time contour plots

## Usage

``` r
pr_get_NRSEnvContour(Data = "Chemistry")
```

## Arguments

- Data:

  Use Chemistry, Pico or Micro, the depth stratified samples

## Value

A dataframe to be used with pr_plot_NRSEnvContour

## Examples

``` r
dat <- pr_get_NRSEnvContour(Data = "Micro")
```
