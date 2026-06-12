# Coastal microbial station locations

An sf object containing coastal microbial (NRS Coastal) station
locations and basic metadata used by plotting and mapping helpers in the
package.

## Usage

``` r
csDAT
```

## Format

An object of class `sf` (a data.frame with spatial geometry). The object
contains one row per station and columns including at least:

- StationName:

  station name (character)

- StationCode:

  station code (character)

- Longitude:

  decimal longitude (numeric)

- Latitude:

  decimal latitude (numeric)

- State:

  state (factor)

- geometry:

  point geometry (sfc_POINT)

## Source

Constructed from Coastal microbial station data via
`pr_get_data(Survey = "Coastal", Type = "Micro")` in
`data-raw/DATASET.R`.
