# Format a parameter string for plotting

Format a parameter string for plotting

## Usage

``` r
pr_relabel(s, style = "ggplot", named = FALSE, quiet = FALSE)
```

## Arguments

- s:

  A string for reformatting

- style:

  The style of plotting the string will be used for

- named:

  Should the output be an `unnamed` (FALSE) or `named` (TRUE) vector?
  Only available for `style = "simple"` and `style = "plotly"`

- quiet:

  Logical. If `TRUE`, suppresses warnings about missing replacements.
  Default is `FALSE`.

## Value

A reformatted expression call (ggplot)
