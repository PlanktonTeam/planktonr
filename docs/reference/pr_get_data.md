# Get plankton and biogeochemical data from IMOS surveys

A unified interface for retrieving plankton abundance, biovolume, and
biogeochemical data from IMOS marine monitoring programs. This function
consolidates multiple survey-specific functions into a single entry
point.

## Usage

``` r
pr_get_data(
  Survey = "NRS",
  Type = "Phytoplankton",
  Variable = NULL,
  Subset = NULL,
  Format = "all"
)
```

## Arguments

- Survey:

  Survey program to retrieve data from:

  - `"NRS"` - National Reference Stations (default). Coastal monitoring
    stations around Australia with monthly sampling.

  - `"CPR"` - Continuous Plankton Recorder. Ship-towed sampler providing
    broad-scale spatial coverage along shipping routes.

  - `"SOTS"` - Southern Ocean Time Series. Deep-water mooring site south
    of Tasmania for monitoring Southern Ocean ecosystems.

  - `"Coastal"` - Coastal Seas microbial monitoring sites.

  - `"GO-SHIP"` - Global Ocean Ship-based Hydrographic Investigations
    Program.

  - `"HAB"` - Phytoplankton monitoring data from state based and seafood
    industry monitoring programs

- Type:

  Data type to retrieve (case-insensitive for
  Phytoplankton/Zooplankton):

  - `"Phytoplankton"` - Phytoplankton abundance or biovolume (NRS, CPR,
    SOTS)

  - `"Zooplankton"` - Zooplankton abundance (NRS, CPR, SOTS)

  - `"Chemistry"` - Water chemistry including nutrients (NRS, Coastal)

  - `"Pigments"` - HPLC photosynthetic pigments (NRS only)

  - `"Pico"` - Picophytoplankton from flow cytometry (NRS only)

  - `"Micro"` - Microbial community data (NRS, Coastal, GO-SHIP)

  - `"TSS"` - Total Suspended Solids (NRS only)

  - `"CTD"` - CTD profiles: temperature, salinity, fluorescence (NRS
    only)

- Variable:

  Variable to retrieve (required for Phytoplankton/Zooplankton):

  - `"abundance"` - Cell or individual counts

  - `"biovolume"` - Cell biovolume in cubic micrometres (Phytoplankton
    only)

- Subset:

  Data aggregation level (required for Phytoplankton/Zooplankton):

  - `"raw"` - Raw taxonomic data as identified by analysts

  - `"htg"` - Higher taxonomic groups (e.g., diatoms, dinoflagellates)

  - `"genus"` - Aggregated to genus level

  - `"species"` - Aggregated to species level

  - `"copepods"` - Copepod data only (Zooplankton only)

- Format:

  Output format for Pigments data:

  - `"all"` - All individual pigment concentrations (default)

  - `"binned"` - Pigments grouped into functional classes

## Value

A dataframe containing the requested data. Structure varies by data type
(see Details).

## Details

### Available Data by Survey

|  |  |
|----|----|
| Survey | Available Types |
| NRS | Phytoplankton, Zooplankton, Chemistry, Pigments, Pico, Micro, TSS, CTD |
| CPR | Phytoplankton, Zooplankton |
| SOTS | Phytoplankton, Zooplankton |
| HAB | Phytoplankton |
| Coastal | Micro, Chemistry |
| GO-SHIP | Micro |

### Return Format

The structure of the returned dataframe varies by data type:

**Wide format** (taxa/variables as columns):

- Phytoplankton, Zooplankton: Taxa names as columns, samples as rows

- CTD: Measurement variables as columns, depth bins as rows

**Long format** (Parameters and Values columns):

- Chemistry, Pigments, Pico, Micro, TSS: Standardised long format with
  `Parameters` column containing variable names and `Values` column
  containing measurements

### Parameter Requirements

- `Variable` and `Subset` are required for Phytoplankton and Zooplankton

- `Format` is only used for Pigments (defaults to "all")

- Unused parameters will generate a warning

## See also

[`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
for sampling trip metadata,
[`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
for derived ecological indices,
[`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
for station and taxonomic information

## Examples

``` r
# Get NRS phytoplankton abundance data at genus level
dat <- pr_get_data(Survey = "NRS", Type = "Phytoplankton",
                   Variable = "abundance", Subset = "genus")

# Get CPR zooplankton data
dat <- pr_get_data(Survey = "CPR", Type = "Zooplankton",
                   Variable = "abundance", Subset = "htg")

# Get NRS water chemistry
dat <- pr_get_data(Survey = "NRS", Type = "Chemistry")

# Get NRS pigments in binned format
dat <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")

# Get NRS CTD profiles
dat <- pr_get_data(Survey = "NRS", Type = "CTD")

# Get GO-SHIP microbial data
dat <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")

# Get Coastal Seas chemistry data
dat <- pr_get_data(Survey = "Coastal", Type = "Chemistry")

# Get HAB phytoplankton data
dat <- pr_get_data(Survey = "HAB", Type = "Phytoplankton",
                   Variable = "abundance", Subset = "genus")
```
