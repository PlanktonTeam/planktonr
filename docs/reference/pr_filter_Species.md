# Remove incorrect species names from dataframe

Remove incorrect species names from dataframe

## Usage

``` r
pr_filter_Species(df)
```

## Arguments

- df:

  A dataframe with species names

## Value

A dataframe with all correct species names

## Examples

``` r
df <- data.frame(Species = c("IncorrectSpecies cf.", "CorrectSpecies1", NA,
              "CorrectSpecies2", "Incorrect spp., Incorrect/Species"))
df <- pr_filter_Species(df)
```
