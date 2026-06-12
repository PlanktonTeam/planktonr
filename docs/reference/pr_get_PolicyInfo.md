# Load policy-relevant plankton indicators and Essential Ocean Variables

**\[deprecated\]**

`pr_get_PolicyInfo()` was deprecated in planktonr 0.7.0 in favour of
[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
which provides a unified interface for both plankton taxonomic
information and survey policy information.

## Usage

``` r
pr_get_PolicyInfo(Survey = "NRS", ...)
```

## Arguments

- Survey:

  "NRS", "CPR", or "SOTS"

- ...:

  Additional arguments (unused, for backward compatibility)

## Value

A dataframe with policy information

## See also

[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for the preferred interface

## Examples

``` r
if (FALSE) { # \dontrun{
# Instead of:
dat <- pr_get_PolicyInfo("CPR")

# Use:
dat <- pr_get_info(Source = "CPR")
} # }
```
