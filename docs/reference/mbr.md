# Marine bioregions (polygons and colours)

An sf object containing marine bioregion polygons and associated colour
codes used for map rendering and region-based plotting in the package.

## Usage

``` r
mbr
```

## Format

An object of class `sf` (a data.frame with spatial geometry). The object
contains one row per bioregion and columns including at least:

- REGION:

  bioregion name (character)

- Colour:

  hex colour code associated with the region (character)

- geometry:

  polygon geometry (sfc_MULTIPOLYGON or sfc_POLYGON)

## Source

Constructed from the marine regions shapefiles and supporting data in
`data-raw/` by the `data-raw/DATASET.R` script.
