# Access derived ecological indices for timeseries and climatology plots

Get pre-calculated ecological indices and summary statistics from IMOS
plankton data. These indices include biodiversity metrics (Shannon
diversity, evenness), biomass estimates, abundance measures, and
community structure indicators.

## Usage

``` r
pr_get_Indices(Survey = "CPR", Type = "Phytoplankton", ...)
```

## Arguments

- Survey:

  Survey type:

  - `"NRS"` - National Reference Stations (coastal fixed-point stations)

  - `"CPR"` - Continuous Plankton Recorder (ship-based transects)

  - `"SOTS"` - Southern Ocean Time Series (calculated from NRS data for
    SOTS station)

  - `"HAB"` - Phytoplankton monitoring data from state based and seafood
    industry monitoring programs

- Type:

  Data type:

  - `"Phytoplankton"` - Phytoplankton community indices

  - `"Zooplankton"` - Zooplankton community indices

  - `"Water"` - Physical and chemical water properties

- ...:

  Additional variables passed to
  [`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md).
  Currently supports:

  - `near_dist_km` - Distance in kilometres around each bioregion
    boundary to pad the allocation of CPR samples to bioregions (useful
    for samples near boundaries)

## Value

A dataframe in long format with columns:

- For NRS, SOTS & HAB: `StationCode`, `StationName`, `SampleTime_Local`,
  `Latitude`, `Longitude`

- For CPR: `BioRegion`, `SampleTime_Local`, `Latitude`, `Longitude`

- Common: `Year_Local`, `Month_Local`, `Parameters` (index name),
  `Values` (index value)

## Details

### Available Parameters by Survey and Type

#### NRS Zooplankton:

- `Biomass_mgm3` - Total zooplankton biomass (mg/m³)

- `AshFreeBiomass_mgm3` - Ash-free dry weight biomass (mg/m³)

- `ZoopAbundance_m3` - Total zooplankton abundance (individuals/m³)

- `CopeAbundance_m3` - Copepod abundance (individuals/m³)

- `AvgTotalLengthCopepod_mm` - Mean copepod body length (mm)

- `OmnivoreCarnivoreCopepodRatio` - Ratio of omnivorous/carnivorous to
  total copepods

- `NoCopepodSpecies_Sample` - Number of copepod species per sample

- `ShannonCopepodDiversity` - Shannon diversity index for copepods

- `CopepodEvenness` - Pielou's evenness for copepods

#### CPR Zooplankton:

- `BiomassIndex_mgm3` - Zooplankton biomass index (mg/m³)

- `ZoopAbundance_m3`, `CopeAbundance_m3`, `AvgTotalLengthCopepod_mm`,
  `OmnivoreCarnivoreCopepodRatio`, `NoCopepodSpecies_Sample`,
  `ShannonCopepodDiversity`, `CopepodEvenness` (as above)

#### NRS Phytoplankton:

- `PhytoBiomassCarbon_pgL` - Phytoplankton carbon biomass (pg/L)

- `PhytoAbundance_CellsL` - Phytoplankton abundance (cells/L)

- `DiatomDinoflagellateRatio` - Ratio of diatoms to
  diatoms+dinoflagellates

- `AvgCellVol_um3` - Mean cell volume (µm³)

- `NoPhytoSpecies_Sample` - Number of phytoplankton species per sample

- `ShannonPhytoDiversity` - Shannon diversity index for phytoplankton

- `PhytoEvenness` - Pielou's evenness for phytoplankton

- Plus diversity metrics for diatoms and dinoflagellates separately

#### CPR Phytoplankton:

- `PCI` - Phytoplankton Colour Index (visual estimate of phytoplankton
  biomass)

- `PhytoBiomassCarbon_pgm3` - Carbon biomass (pg/m³)

- `PhytoAbundance_Cellsm3` - Abundance (cells/m³)

- Plus similar metrics as NRS phytoplankton

#### NRS Water:

- `Secchi_m` - Secchi depth (m)

- `MLDtemp_m` - Mixed layer depth from temperature (m)

- `MLDsal_m` - Mixed layer depth from salinity (m)

- `DCM_m` - Deep chlorophyll maximum depth (m)

- `CTDTemperature_degC` - Mean temperature in top 10m (°C)

- `CTDSalinity_PSU` - Mean salinity in top 10m (PSU)

- `CTDChlaF_mgm3` - Mean chlorophyll fluorescence in top 10m (mg/m³)

#### CPR Water:

- `PCI` - Phytoplankton Colour Index

### SOTS & HAB Data

For SOTS (Southern Ocean Time Series) and HAB (Coastal Phytoplankton),
phytoplankton indices are calculated from raw NRS data as they are not
provided directly by AODN. For SOTS only samples from the LM (Light
Microscopy) method at depths \<50m are included.

## See also

[`pr_filter_data()`](https://planktonteam.github.io/planktonr/reference/pr_filter_data.md)
to filter the output for specific parameters and stations,
[`pr_plot_Trends()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Trends.md),
[`pr_plot_TimeSeries()`](https://planktonteam.github.io/planktonr/reference/pr_plot_TimeSeries.md),
[`pr_plot_Climatology()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Climatology.md)
for visualisation

## Examples

``` r
# Get NRS phytoplankton indices
dat <- pr_get_Indices("NRS", "Phytoplankton")
unique(dat$Parameters)
#>  [1] "PhytoBiomassCarbon_pgL"    "PhytoAbundance_CellsL"    
#>  [3] "DiatomDinoflagellateRatio" "AvgCellVol_um3"           
#>  [5] "NoPhytoSpecies_Sample"     "ShannonPhytoDiversity"    
#>  [7] "PhytoEvenness"             "NoDiatomSpecies_Sample"   
#>  [9] "ShannonDiatomDiversity"    "DiatomEvenness"           
#> [11] "NoDinoSpecies_Sample"      "ShannonDinoDiversity"     
#> [13] "DinoflagellateEvenness"   

# Get CPR zooplankton indices with expanded bioregion boundaries
dat <- pr_get_Indices("CPR", "Zooplankton", near_dist_km = 250)

# Get water properties from NRS
dat <- pr_get_Indices("NRS", "Water")

# Get HAB phytoplankton indices
dat <- pr_get_Indices("HAB", "Phytoplankton", Subset = 'Genus')

# Filter for specific parameter and stations
dat <- pr_get_Indices("NRS", "Zooplankton") %>%
  pr_filter_data("Biomass_mgm3", c("MAI", "PHB"))
```
