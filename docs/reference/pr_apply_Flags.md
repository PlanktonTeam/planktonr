# Remove flagged data points based on IMOS quality control flags

Remove flagged data points based on IMOS quality control flags

## Usage

``` r
pr_apply_Flags(df, flag_col)
```

## Arguments

- df:

  A dataframe containing data with associated flags

- flag_col:

  A string specifying the column with the flag. Optional. If specified,
  all rows will be filter by the flag.

## Value

A dataframe with flagged data removed

## Examples

``` r
df <- data.frame(SST = c(27.4, 28.9, 45), SST_Flag = c(1, 1, 4))
df <- pr_apply_Flags(df)
```
