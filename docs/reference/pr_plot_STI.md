# Plot Species Temperature Index (STI) kernel density distributions

Plot Species Temperature Index (STI) kernel density distributions

## Usage

``` r
pr_plot_STI(df)
```

## Arguments

- df:

  dataframe as output of pr_get_STIdata() filtered for one species

## Value

plot of STI kernel density

## Examples

``` r
df <- data.frame(SST = runif(24, 5, 25),
                 Project = c(rep("CPR", 12), rep("NRS", 12)),
                 Species_m3 = runif(24, 0.1, 10),
                 Species = 'Acartia danae')

plot <- pr_plot_STI(df)
```
