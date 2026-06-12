# Load Essential Ocean Variables (EOVs) for plankton biomass and diversity

Load Essential Ocean Variables (EOVs) for plankton biomass and diversity

## Usage

``` r
pr_get_EOVs(Survey = "NRS", ...)
```

## Arguments

- Survey:

  "NRS" or "CPR" or "LTM"

- ...:

  to allow use of join when used within another function

## Value

A dataframe with policy data

## Examples

``` r
dat <- pr_get_EOVs("NRS")
```
