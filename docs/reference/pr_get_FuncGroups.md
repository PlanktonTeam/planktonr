# Aggregate plankton data by functional groups

Aggregate plankton data into major functional groups for community
structure analysis. Functional groups represent taxa with similar
ecological roles or morphological characteristics (e.g., diatoms,
dinoflagellates, copepods).

## Usage

``` r
pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton", ...)
```

## Arguments

- Survey:

  Survey type:

  - `"NRS"` - National Reference Stations

  - `"CPR"` - Continuous Plankton Recorder

  - `"SOTS"` - Southern Ocean Time Series

- Type:

  Plankton type:

  - `"Phytoplankton"` - Includes centric diatoms, pennate diatoms,
    dinoflagellates, cyanobacteria, and other groups

  - `"Zooplankton"` - Includes copepods, appendicularians, molluscs,
    cladocerans, chaetognaths, thaliaceans, and other groups

- ...:

  Additional variables passed to
  [`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md).
  For CPR data, you can use:

  - `near_dist_km` - Distance in kilometres to pad bioregion boundaries

## Value

A dataframe in long format with columns for location/station, date,
functional group (`Parameters`), and abundance (`Values`). Suitable for
use with
[`pr_plot_tsfg()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsfg.md)
and other functional group visualisation functions.

## Details

### Phytoplankton Functional Groups

The function aggregates phytoplankton into five major groups:

- Centric diatoms - Radially symmetrical diatoms, typically
  bloom-forming

- Pennate diatoms - Bilaterally symmetrical diatoms

- Dinoflagellates - Flagellated protists, some toxic

- Cyanobacteria - Photosynthetic bacteria (e.g., Synechococcus,
  Trichodesmium)

- Other - All remaining groups (flagellates, ciliates, etc.)

### Zooplankton Functional Groups

The function aggregates zooplankton into seven major groups:

- Copepods - Dominant group of marine zooplankton

- Appendicularians - Gelatinous filter feeders (larvaceans)

- Molluscs - Primarily pteropods (sea butterflies, sea angels)

- Cladocerans - Water fleas (e.g., Penilia, Evadne)

- Chaetognaths - Arrow worms (predatory)

- Thaliaceans - Gelatinous filter feeders (salps, doliolids, pyrosomes)

- Other - All remaining groups

Data are based on higher taxonomic group (htg) abundance and aggregated
by sample. Port Hacking 4 samples are excluded from NRS data.

## See also

[`pr_plot_tsfg()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsfg.md)
for plotting functional group time series,
[`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
for community-level indices

## Examples

``` r
# Get NRS zooplankton functional groups
NRSfgz <- pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton")

# Get NRS phytoplankton functional groups
NRSfgp <- pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton")

# Get CPR functional groups with expanded bioregion boundaries
CPRfgz <- pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250)
CPRfgp <- pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton")

# Examine the functional groups present
unique(NRSfgp$Parameters)
#> [1] Centric diatom Pennate diatom Dinoflagellate Cyanobacteria  Other         
#> Levels: Centric diatom Pennate diatom Dinoflagellate Cyanobacteria Other
```
