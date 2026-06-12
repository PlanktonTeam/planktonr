# Download raw unprocessed data files directly from IMOS S3 storage

This function is not intended for use by most people. It downloads the
raw data file from IMOS with NO QC or data manipulation done. User
beware. It can be used to view the raw data that IMOS provides behind
the scenes.

## Usage

``` r
pr_get_Raw(file)
```

## Arguments

- file:

  The filename of the requested data

## Value

A dataframe of raw IMOS data

## Details

Options for `file` include: "bgc_chemistry_data", "bgc_pigments_data",
"bgc_picoplankton_data", "bgc_tss_data".

## Examples

``` r
dat <- pr_get_Raw("bgc_chemistry_data")
dat <- pr_get_Raw("bgc_pigments_data")
dat <- pr_get_Raw("bgc_picoplankton_data")
dat <- pr_get_Raw("bgc_tss_data")
```
