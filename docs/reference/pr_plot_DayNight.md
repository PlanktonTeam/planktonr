# Visualise diel vertical migration patterns showing day vs night abundances

Visualise diel vertical migration patterns showing day vs night
abundances

## Usage

``` r
pr_plot_DayNight(df)
```

## Arguments

- df:

  dataframe as output of pr_get_DayNight() filtered for one species

## Value

plot of relative day and night abundances

## Examples

``` r
df <- pr_get_DayNight()
plot <- pr_plot_DayNight(df)
```
