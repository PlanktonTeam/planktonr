# Create harmonic functions for circular predictors

For use in models to create a circular predictor

## Usage

``` r
pr_harmonic(theta, k = 4)
```

## Arguments

- theta:

  Parameter to model in radians

- k:

  degrees of freedom

## Value

A harmonic function

## Details

This function is for use in models to create a circular predictor

## Examples

``` r
theta = 2
k = 1
df <- pr_harmonic(theta = 2, k = 1)
```
