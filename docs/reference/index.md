# Package index

## Load Data

Functions to access data from the AODN.

- [`pr_get_data()`](https://planktonteam.github.io/planktonr/reference/pr_get_data.md)
  : Get plankton and biogeochemical data from IMOS surveys
- [`pr_get_trips()`](https://planktonteam.github.io/planktonr/reference/pr_get_trips.md)
  : Get sampling trip metadata
- [`pr_get_Indices()`](https://planktonteam.github.io/planktonr/reference/pr_get_Indices.md)
  : Access derived ecological indices for timeseries and climatology
  plots
- [`pr_get_EOVs()`](https://planktonteam.github.io/planktonr/reference/pr_get_EOVs.md)
  : Load Essential Ocean Variables (EOVs) for plankton biomass and
  diversity
- [`pr_get_CTI()`](https://planktonteam.github.io/planktonr/reference/pr_get_CTI.md)
  : Calculate Community Temperature Index (CTI) for plankton samples
- [`pr_get_STI()`](https://planktonteam.github.io/planktonr/reference/pr_get_STI.md)
  : Get STI kernel density for each species
- [`pr_get_STIdata()`](https://planktonteam.github.io/planktonr/reference/pr_get_STIdata.md)
  : Get data for STI plots of species abundance
- [`pr_get_FuncGroups()`](https://planktonteam.github.io/planktonr/reference/pr_get_FuncGroups.md)
  : Aggregate plankton data by functional groups
- [`pr_get_TaxaAccum()`](https://planktonteam.github.io/planktonr/reference/pr_get_TaxaAccum.md)
  : Get taxa accumulation data for plotting
- [`pr_get_FreqMap()`](https://planktonteam.github.io/planktonr/reference/pr_get_FreqMap.md)
  : Prepare species occurrence data for frequency mapping
- [`pr_get_LFData()`](https://planktonteam.github.io/planktonr/reference/pr_get_LFData.md)
  : Get NRS larval fish abundance data
- [`pr_get_LTnuts()`](https://planktonteam.github.io/planktonr/reference/pr_get_LTnuts.md)
  : Get NRS long term nutrient timeseries data
- [`pr_get_NRSEnvContour()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSEnvContour.md)
  : Prepare biogeochemical data for depth-time contour plots
- [`pr_get_info()`](https://planktonteam.github.io/planktonr/reference/pr_get_info.md)
  : Load plankton taxonomic or survey policy information
- [`pr_get_ProgressMapData()`](https://planktonteam.github.io/planktonr/reference/pr_get_ProgressMapData.md)
  : Prepare sampling coverage data for IMOS progress map visualisation
- [`pr_get_DataLocs()`](https://planktonteam.github.io/planktonr/reference/pr_get_DataLocs.md)
  : Extract sample locations and dates for satellite data matching
- [`pr_get_Raw()`](https://planktonteam.github.io/planktonr/reference/pr_get_Raw.md)
  : Download raw unprocessed data files directly from IMOS S3 storage
- [`pr_get_s3()`](https://planktonteam.github.io/planktonr/reference/pr_get_s3.md)
  : Download raw data files from IMOS internal S3 storage (advanced
  users)
- [`pr_get_DayNight()`](https://planktonteam.github.io/planktonr/reference/pr_get_DayNight.md)
  : Get data for plots of species abundance by day and night using CPR
  data
- [`pr_get_SOTSMoorData()`](https://planktonteam.github.io/planktonr/reference/pr_get_SOTSMoorData.md)
  : Get data available from SOTS moorings (limited to use in BOO atm,
  other variables could be added, select from pr_get_SOTSvariables)
- [`pr_get_SOTSvariables()`](https://planktonteam.github.io/planktonr/reference/pr_get_SOTSvariables.md)
  : Get data files available from SOTS moorings
- [`pr_get_PCIData()`](https://planktonteam.github.io/planktonr/reference/pr_get_PCIData.md)
  : Get Phytoplankton Colour Index (PCI) data from CPR samples
- [`pr_get_SatData()`](https://planktonteam.github.io/planktonr/reference/pr_get_SatData.md)
  : Load satellite-derived environmental data matched to plankton
  sampling locations

## Plotting

Functions for plotting.

- [`pr_plot_CPRmap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_CPRmap.md)
  : Title Sidebar panel plot of selected CPR bioregions
- [`pr_plot_Climatology()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Climatology.md)
  : Plot climatologies showing seasonal or interannual patterns
- [`pr_plot_DayNight()`](https://planktonteam.github.io/planktonr/reference/pr_plot_DayNight.md)
  : Visualise diel vertical migration patterns showing day vs night
  abundances
- [`pr_plot_EOVs()`](https://planktonteam.github.io/planktonr/reference/pr_plot_EOVs.md)
  : Essential Ocean Variables (EOV) plot for reporting
- [`pr_plot_Enviro()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Enviro.md)
  : Plot environmental data with depth profiles
- [`pr_plot_FreqMap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_FreqMap.md)
  : Map seasonal occurrence frequency for individual plankton species
- [`pr_plot_Gantt()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Gantt.md)
  : Plot Gantt Chart showing plankton sampling status
- [`pr_plot_NRSEnvContour()`](https://planktonteam.github.io/planktonr/reference/pr_plot_NRSEnvContour.md)
  : Plot environmental contours showing depth-time patterns
- [`pr_plot_NRSmap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_NRSmap.md)
  : Create map showing selected NRS station locations
- [`pr_plot_PCImap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_PCImap.md)
  : Map Phytoplankton Colour Index (PCI) from CPR samples around
  Australia
- [`pr_plot_PieFG()`](https://planktonteam.github.io/planktonr/reference/pr_plot_PieFG.md)
  : Create pie charts showing functional group composition
- [`pr_plot_ProgressMap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_ProgressMap.md)
  : Create interactive map showing IMOS plankton sampling coverage and
  progress
- [`pr_plot_STI()`](https://planktonteam.github.io/planktonr/reference/pr_plot_STI.md)
  : Plot Species Temperature Index (STI) kernel density distributions
- [`pr_plot_TaxaAccum()`](https://planktonteam.github.io/planktonr/reference/pr_plot_TaxaAccum.md)
  : Plot taxonomic accumulation curves showing cumulative species
  discovery over time
- [`pr_plot_TimeSeries()`](https://planktonteam.github.io/planktonr/reference/pr_plot_TimeSeries.md)
  : Plot basic timeseries of plankton indices
- [`pr_plot_Trends()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Trends.md)
  : Plot temporal trends in plankton data with fitted lines
- [`pr_plot_Voyagemap()`](https://planktonteam.github.io/planktonr/reference/pr_plot_Voyagemap.md)
  : Create map showing CPR voyage tracks and sampling locations
- [`pr_plot_box()`](https://planktonteam.github.io/planktonr/reference/pr_plot_box.md)
  : Simple boxplot function using common NRS colouring
- [`pr_plot_latitude()`](https://planktonteam.github.io/planktonr/reference/pr_plot_latitude.md)
  : Create Hovmöller diagram showing latitudinal patterns over time
- [`pr_plot_scatter()`](https://planktonteam.github.io/planktonr/reference/pr_plot_scatter.md)
  : Simple function to scatter 2 data columns using common NRS colouring
- [`pr_plot_tsclimate()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsclimate.md)
  : Combined timeseries and climatology plots
- [`pr_plot_tsfg()`](https://planktonteam.github.io/planktonr/reference/pr_plot_tsfg.md)
  : Time series plot of functional group composition

## Utilities

Helper functions for data manipulation and analysis.

- [`pr_add_Bioregions()`](https://planktonteam.github.io/planktonr/reference/pr_add_Bioregions.md)
  : Assign Australian Marine Bioregions to sample locations

- [`pr_add_Carbon()`](https://planktonteam.github.io/planktonr/reference/pr_add_Carbon.md)
  : Add Carbon concentration to phytoplankton dataframe

- [`pr_add_StationCode()`](https://planktonteam.github.io/planktonr/reference/pr_add_StationCode.md)
  : Add NRS station codes to data

- [`pr_add_StationName()`](https://planktonteam.github.io/planktonr/reference/pr_add_StationName.md)
  : Add NRS station full names to data

- [`pr_apply_Flags()`](https://planktonteam.github.io/planktonr/reference/pr_apply_Flags.md)
  : Remove flagged data points based on IMOS quality control flags

- [`pr_filter_data()`](https://planktonteam.github.io/planktonr/reference/pr_filter_data.md)
  : Filter data for plotting functions

- [`pr_filter_NRSStations()`](https://planktonteam.github.io/planktonr/reference/pr_filter_NRSStations.md)
  :

  Filter dataframe on `StationCode` to return only NRS stations

- [`pr_filter_Species()`](https://planktonteam.github.io/planktonr/reference/pr_filter_Species.md)
  : Remove incorrect species names from dataframe

- [`pr_harmonic()`](https://planktonteam.github.io/planktonr/reference/pr_harmonic.md)
  : Create harmonic functions for circular predictors

- [`pr_make_climatology()`](https://planktonteam.github.io/planktonr/reference/pr_make_climatology.md)
  : To produce the climatology for plotting

- [`pr_model_data()`](https://planktonteam.github.io/planktonr/reference/pr_model_data.md)
  : Fit linear models to plankton time series data

- [`pr_remove_outliers()`](https://planktonteam.github.io/planktonr/reference/pr_remove_outliers.md)
  : Remove statistical outliers from plankton data

- [`pr_get_coeffs()`](https://planktonteam.github.io/planktonr/reference/pr_get_coeffs.md)
  : Extract tidy model coefficients for trend analysis

- [`theme_pr()`](https://planktonteam.github.io/planktonr/reference/theme_pr.md)
  : Define theme_pr() function

## Satellite Matching

Functions for matching plankton samples to satellite data.

- [`pr_match_Altimetry()`](https://planktonteam.github.io/planktonr/reference/pr_match_Altimetry.md)
  : Match plankton samples to satellite altimetry data (sea level,
  currents)
- [`pr_match_GHRSST()`](https://planktonteam.github.io/planktonr/reference/pr_match_GHRSST.md)
  : Match plankton sample locations to GHRSST sea surface temperature
  data
- [`pr_match_MODIS()`](https://planktonteam.github.io/planktonr/reference/pr_match_MODIS.md)
  : Match plankton samples to MODIS ocean colour data (chlorophyll, PAR,
  SST)

## Data

Built-in datasets included with the package.

- [`csDAT`](https://planktonteam.github.io/planktonr/reference/csDAT.md)
  : Coastal microbial station locations
- [`mbr`](https://planktonteam.github.io/planktonr/reference/mbr.md) :
  Marine bioregions (polygons and colours)

## Deprecated

These functions are deprecated and will be removed in a future version.
Please use \[pr_get_data()\], \[pr_get_trips()\], or \[pr_get_info()\]
instead.

- [`pr_get_NRSTrips()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSTrips.md)
  **\[deprecated\]** : Import NRS sampling trip metadata
- [`pr_get_CPRTrips()`](https://planktonteam.github.io/planktonr/reference/pr_get_CPRTrips.md)
  **\[deprecated\]** : Get CPR sampling trip metadata
- [`pr_get_NRSData()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSData.md)
  **\[deprecated\]** : Import NRS plankton data
- [`pr_get_CPRData()`](https://planktonteam.github.io/planktonr/reference/pr_get_CPRData.md)
  **\[deprecated\]** : Import CPR plankton data
- [`pr_get_NRSChemistry()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSChemistry.md)
  **\[deprecated\]** : Load NRS chemistry data
- [`pr_get_NRSPigments()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSPigments.md)
  **\[deprecated\]** : Load NRS pigments data
- [`pr_get_NRSPico()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSPico.md)
  **\[deprecated\]** : Load NRS picophytoplankton data
- [`pr_get_NRSMicro()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSMicro.md)
  **\[deprecated\]** : Load microbial data
- [`pr_get_NRSTSS()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSTSS.md)
  **\[deprecated\]** : Load NRS Total Suspended Solids data
- [`pr_get_NRSCTD()`](https://planktonteam.github.io/planktonr/reference/pr_get_NRSCTD.md)
  **\[deprecated\]** : Load NRS CTD profile data
- [`pr_get_CSChem()`](https://planktonteam.github.io/planktonr/reference/pr_get_CSChem.md)
  **\[deprecated\]** : Load Coastal Seas chemistry and nutrient data
- [`pr_get_SpeciesInfo()`](https://planktonteam.github.io/planktonr/reference/pr_get_SpeciesInfo.md)
  **\[deprecated\]** : Get species information table for Phytoplankton
  and Zooplankton
- [`pr_get_PlanktonInfo()`](https://planktonteam.github.io/planktonr/reference/pr_get_PlanktonInfo.md)
  **\[deprecated\]** : Load taxonomic information and trait data for
  plankton species
- [`pr_get_PolicyInfo()`](https://planktonteam.github.io/planktonr/reference/pr_get_PolicyInfo.md)
  **\[deprecated\]** : Load policy-relevant plankton indicators and
  Essential Ocean Variables
- [`pr_get_Stations()`](https://planktonteam.github.io/planktonr/reference/pr_get_Stations.md)
  **\[deprecated\]** : Import NRS Station information
