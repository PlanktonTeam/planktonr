# Create Hovmöller diagram showing latitudinal patterns over time

Create Hovmöller diagram showing latitudinal patterns over time

## Usage

``` r
pr_plot_latitude(df, na.fill = TRUE)
```

## Arguments

- df:

  dataframe of latitudinal series

- na.fill:

  TRUE, FALSE or function like mean to fill in gaps in data

## Value

patchwork object

## Examples

``` r
df <- pr_get_data(Survey = 'GO-SHIP', Type = 'Micro')
df <- df %>% dplyr::filter(Parameters == 'Bacteria_unique_ASVs',
SampleDepth_m < 101)
pr_plot_latitude(df, na.fill = mean)
#> Warning: Imputing missing values.
```
