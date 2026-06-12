# Filter data for plotting functions

Filter plankton data by parameter and location

## Usage

``` r
pr_filter_data(dat, Parameter = "Biomass_mgm3", StationRegion = "NSI")
```

## Arguments

- dat:

  A dataframe from
  [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md),
  [`pr_get_EOVs()`](https://planktonteam.github.io/planktonr/reference/pr_get_EOVs.md),
  or similar functions

- Parameter:

  Character string or vector of parameter names to retain. Common
  parameters include:

  - **Biomass**: `"Biomass_mgm3"`, `"BiomassIndex_mgm3"`,
    `"PhytoBiomassCarbon_pgL"`

  - **Abundance**: `"Abundance_m3"`, `"AbundanceIndex_Samplekm"`,
    `"TotalCopepodAbundance_m3"`

  - **Diversity**: `"ShannonDiversity"`, `"TotalTaxa_Sample"`,
    `"Richness"`

- StationRegion:

  Character string or vector of locations to retain:

  - **For NRS data**: Station codes like `"NSI"`, `"PHB"`, `"MAI"`,
    `"ROT"`, `"YON"`, `"DAR"`, `"KAI"`, `"ESP"`, `"NIN"`

  - **For CPR data**: Bioregions like `"Temperate East"`,
    `"South-east"`, `"South-west"`, `"North-west"`, `"Coral Sea"`

## Value

A filtered dataframe maintaining the same structure as input

## Details

Convenience function to filter index data to specific parameters and
locations before plotting or analysis. Automatically detects whether
data uses station codes (NRS) or bioregions (CPR) and filters
accordingly.

This function streamlines the common workflow of filtering data before
creating plots or running analyses. It:

- Detects data type automatically (NRS vs CPR)

- Filters on `Parameters` column to match `Parameter` argument

- Filters on `StationCode` (NRS) or `BioRegion` (CPR) to match
  `StationRegion` argument

- Validates inputs to catch common errors

### When to Use

Use this function when:

- Creating plots for specific stations or regions

- Comparing multiple parameters at the same location(s)

- Preparing data for statistical analysis on a subset of locations

### Multiple Selections

Both `Parameter` and `StationRegion` accept vectors, enabling:

- Multiple parameters at one station:
  `Parameter = c("Biomass_mgm3", "Abundance_m3"), StationRegion = "NSI"`

- One parameter at multiple stations:
  `Parameter = "Biomass_mgm3", StationRegion = c("NSI", "PHB", "MAI")`

- Multiple parameters at multiple stations: vectors for both arguments

## See also

- [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
  for loading data before filtering

- [`pr_plot_TimeSeries()`](https://planktonteam.github.io/planktonr/reference/pr_plot_TimeSeries.md),
  [`pr_plot_Trends()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Trends.md),
  [`pr_plot_Climatology()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Climatology.md)
  for plotting filtered data

## Examples

``` r
# Filter CPR data to biomass in two bioregions
dat <- pr_get_Indices("CPR", "Zooplankton") %>%
  pr_filter_data("BiomassIndex_mgm3", c("North", "South-west"))

# Filter NRS data to phytoplankton carbon at two stations
dat <- pr_get_Indices("NRS", "Phytoplankton") %>%
  pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB"))

# Multiple parameters at one station
dat <- pr_get_Indices("NRS", "Zooplankton") %>%
  pr_filter_data(c("Biomass_mgm3", "Abundance_m3", "ShannonDiversity"), "MAI")
```
