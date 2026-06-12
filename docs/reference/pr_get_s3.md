# Download raw data files from IMOS internal S3 storage (advanced users)

This function is not intended for use by most people. It downloads the
raw data file from IMOS with NO QC or data manipulation done. User
beware. It can be used to view the raw data that IMOS provides behind
the scenes.

## Usage

``` r
pr_get_s3(file)
```

## Arguments

- file:

  The filename of the requested data

## Value

A dataframe of raw IMOS data

## Details

Options for `file` include: "bgc_trip"

## Examples

``` r
dat <- pr_get_s3("bgc_trip")
dat <- pr_get_s3("bgc_zoop_raw")
dat <- pr_get_s3("bgc_phyto_raw")
dat <- pr_get_s3("cpr_phyto_raw")
dat <- pr_get_s3("cpr_zoop_raw")
```
