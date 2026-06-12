# Load plankton taxonomic or survey policy information

A unified function to retrieve reference tables containing either
taxonomic information for phytoplankton/zooplankton species, or
policy-relevant metadata for NRS/CPR surveys.

## Usage

``` r
pr_get_info(Source = "Zooplankton")
```

## Arguments

- Source:

  The type of information to retrieve. Must be one of:

  - `"Zooplankton"` or `"Z"` - Zooplankton taxonomic and trait data

  - `"Phytoplankton"` or `"P"` - Phytoplankton taxonomic and trait data

  - `"NRS"` - National Reference Station policy information

  - `"CPR"` - Continuous Plankton Recorder policy information

  - `"SOTS"` - Southern Ocean Time Series policy information

## Value

A dataframe with information appropriate to the requested source:

- For plankton types: taxonomic and trait information

- For surveys: policy-relevant metadata

## Details

### Plankton Information (Phytoplankton/Zooplankton)

When `Source` is `"Phytoplankton"` or `"Zooplankton"`, returns taxonomic
and trait data including:

- **Taxonomic classification**: Phylum, class, order, family, genus,
  species

- **Size information**: Length and/or width ranges (micrometres)

- **Biovolume**: Estimated cell/organism volume (µm³)

- **Carbon content**: Biomass conversion factors (µg C per individual or
  per cell)

- **Functional groups**: Ecological groupings used in
  [`pr_get_FuncGroups()`](https://planktonteam.github.io/planktonr/reference/pr_get_FuncGroups.md)

- **WoRMS identifiers**: AphiaID for linking to World Register of Marine
  Species

### Survey Policy Information (NRS/CPR/SOTS)

When `Source` is `"NRS"`, `"CPR"`, or `"SOTS"`, returns metadata about
sampling programs including station/region information, geographic
features, and operational status.

## See also

- [`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
  for abundance data

- [`pr_get_FuncGroups()`](https://planktonteam.github.io/planktonr/reference/pr_get_FuncGroups.md)
  which uses functional group assignments

## Examples

``` r
# Get zooplankton trait information
zoo_info <- pr_get_info(Source = "Zooplankton")

# Get phytoplankton trait information
phyto_info <- pr_get_info(Source = "Phytoplankton")

# Get NRS station policy information
nrs_info <- pr_get_info(Source = "NRS")

# Get CPR bioregion information
cpr_info <- pr_get_info(Source = "CPR")
```
