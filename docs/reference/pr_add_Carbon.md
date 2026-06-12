# Add Carbon concentration to phytoplankton dataframe

This is where you write the description

## Usage

``` r
pr_add_Carbon(df, meth)
```

## Arguments

- df:

  Input dataframe with BioVolume

- meth:

  Method for data collection

## Value

Dataframe with Carbon included

## Examples

``` r
df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                          Biovolume_um3L = c(100, 150), Cells_L = c(10, 8))
df <- pr_add_Carbon(df, "NRS")
df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                          BioVolume_um3m3 = c(100, 150), PhytoAbund_m3 = c(10, 8))
df <- pr_add_Carbon(df, "CPR")
```
