# To produce the climatology for plotting

To produce the climatology for plotting

## Usage

``` r
pr_make_climatology(dat, x)
```

## Arguments

- dat:

  output of pr_get_Indices

- x:

  A string of Year, Month, Day, time period of climatology

## Value

dataframe to use in pr_plot_Climatology functions

## Examples

``` r
dat <- data.frame(Month = rep(1:12,10), StationCode = "NSI", Values = runif(120, min=0, max=10))
pr_make_climatology(dat, "Month")
#>    Month StationCode     mean  N       sd        se
#> 1      1         NSI 5.164656 10 3.675964 1.1624418
#> 2      2         NSI 6.564165 10 2.634430 0.8330801
#> 3      3         NSI 3.793594 10 2.290833 0.7244249
#> 4      4         NSI 3.327780 10 2.418098 0.7646697
#> 5      5         NSI 4.423760 10 2.946553 0.9317817
#> 6      6         NSI 4.761376 10 2.916079 0.9221450
#> 7      7         NSI 6.255200 10 3.206539 1.0139968
#> 8      8         NSI 4.725419 10 2.706689 0.8559303
#> 9      9         NSI 3.992213 10 1.876342 0.5933515
#> 10    10         NSI 6.735503 10 1.572810 0.4973661
#> 11    11         NSI 5.442305 10 2.478709 0.7838365
#> 12    12         NSI 4.345199 10 2.567004 0.8117580
```
