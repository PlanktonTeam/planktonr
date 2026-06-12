# Get Phytoplankton Colour Index (PCI) data from CPR samples

Get Phytoplankton Colour Index (PCI) data from CPR samples

## Usage

``` r
pr_get_PCIData()
```

## Value

dataframe of PCI data

## Examples

``` r
head(pr_get_PCIData(),5)
#> --- planktonr_dat Attributes ---
#>   Type: Water
#>   Survey: CPR
#> 
#> # A tibble: 5 × 5
#>   Longitude Latitude Season              BioRegion    PCI
#>       <dbl>    <dbl> <chr>               <fct>      <dbl>
#> 1       135      -38 December - February South-east 0.769
#> 2       134      -38 December - February South-east 1.22 
#> 3       133      -38 December - February South-east 1.04 
#> 4       132      -38 December - February South-east 0.848
#> 5       131      -38 December - February South-east 1    
```
