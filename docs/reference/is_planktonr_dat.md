# Check if an object is of class `planktonr_dat`

This function tests if the given object `x` inherits from the
`planktonr_dat` S3 class. It's a standard way to verify if an object is
of your custom data type.

## Usage

``` r
is_planktonr_dat(x)
```

## Arguments

- x:

  An R object to check.

## Value

A logical value: `TRUE` if `x` is a `planktonr_dat` object, otherwise
`FALSE`.

## Examples

``` r
# Assuming planktonr_dat constructor exists:
my_data <- planktonr_dat(tibble::tibble(a = 1:3, b = 4:6),
                         Type = "Phytoplankton",
                         Survey = "NRS")

# Check if it's a planktonr_dat object
is_planktonr_dat(my_data) # Returns TRUE
#> [1] TRUE

# Check a regular tibble
is_planktonr_dat(tibble::tibble(a = 1:3)) # Returns FALSE
#> [1] FALSE

# Check a data.frame
is_planktonr_dat(data.frame(x = 1)) # Returns FALSE
#> [1] FALSE
```
