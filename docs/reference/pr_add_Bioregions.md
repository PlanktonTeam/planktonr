# Assign Australian Marine Bioregions to sample locations

Add bioregion classification to samples based on their geographic
coordinates. Uses Australian marine bioregion boundaries (IMCRA 4.0)
from the Integrated Marine and Coastal Regionalisation of Australia.

## Usage

``` r
pr_add_Bioregions(dat, near_dist_km = 0)
```

## Arguments

- dat:

  A dataframe containing columns `Longitude` and `Latitude` with
  geographic coordinates in decimal degrees (WGS84)

- near_dist_km:

  Buffer distance (in kilometres) to use when assigning bioregions to
  samples that fall outside boundaries. Default is `0` (no buffer).
  Typical values:

  - `0` - Exact match only, samples outside boundaries are labelled
    "None"

  - `50` - Assigns samples within 50 km of a bioregion

  - `250` - More generous buffer, useful for CPR transects near
    boundaries

## Value

A dataframe with two additional columns:

- `BioRegion`: Name of the marine bioregion

- `Colour`: Hex colour code for that bioregion (for plotting)

## Details

### Bioregion Classification

The function assigns samples to one of the following Australian marine
bioregions:

- **Temperate East**: Coastal waters off NSW and southern Queensland

- **South-east**: Bass Strait and waters off Tasmania

- **South-west**: Southern and western Australian waters

- **North-west**: Waters off northwest Western Australia

- **Coral Sea**: Offshore waters northeast of Queensland

- **None**: Samples that don't fall within any bioregion

### Assignment Method

The function uses spatial operations to:

1.  Convert coordinates to spatial features (sf objects)

2.  Match samples to bioregions using polygon intersection

3.  For samples outside boundaries, find the nearest bioregion within
    `near_dist_km`

4.  Prioritise Coral Sea assignments to handle boundary overlaps

5.  Add a colour column for plotting consistency

### Buffer Distance

The `near_dist_km` parameter is particularly useful for:

- CPR samples collected near bioregion boundaries

- Offshore samples that may fall just outside defined regions

- Creating continuous coverage for transect data

Use larger buffers (e.g., 250 km) for offshore CPR data, smaller or no
buffer for coastal NRS data.

### Data Requirements

Input dataframe must include `Longitude` and `Latitude` columns. The
function handles coordinate transformations automatically.

## See also

- [`pr_get_CPRData()`](https://planktonteam.github.io/planktonr/reference/pr_get_CPRData.md)
  which calls this function internally

- [mbr](https://planktonteam.github.io/planktonr/reference/mbr.md) for
  the marine bioregion spatial data

## Examples

``` r
# Add bioregions with exact matching (no buffer)
dat <- pr_get_Raw("cpr_derived_indices_data") %>%
  pr_add_Bioregions()

# Add bioregions with 250 km buffer for offshore samples
dat <- pr_get_Raw("cpr_derived_indices_data") %>%
  pr_add_Bioregions(near_dist_km = 250)

# Check bioregion assignments
table(dat$BioRegion)
#> 
#>             Coral Sea                  None                 North 
#>                  4105                  1108                   348 
#>            North-west            South-east            South-west 
#>                  1119                 11671                  3620 
#> Southern Ocean Region        Temperate East 
#>                  7783                  7065 
```
