#' Access derived ecological indices for timeseries and climatology plots
#'
#' Get pre-calculated ecological indices and summary statistics from IMOS plankton
#' data. These indices include biodiversity metrics (Shannon diversity, evenness),
#' biomass estimates, abundance measures, and community structure indicators.
#'
#' @param Survey Survey type:
#'   * `"NRS"` - National Reference Stations (coastal fixed-point stations)
#'   * `"CPR"` - Continuous Plankton Recorder (ship-based transects)
#'   * `"SOTS"` - Southern Ocean Time Series (calculated from NRS data for SOTS station)
#' @param Type Data type:
#'   * `"Phytoplankton"` - Phytoplankton community indices
#'   * `"Zooplankton"` - Zooplankton community indices
#'   * `"Water"` - Physical and chemical water properties
#' @param ... Additional variables passed to [pr_add_Bioregions()]. Currently supports:
#'   * `near_dist_km` - Distance in kilometres around each bioregion boundary to pad
#'     the allocation of CPR samples to bioregions (useful for samples near boundaries)
#'
#' @details
#' ## Available Parameters by Survey and Type
#'
#' ### NRS Zooplankton:
#' * `Biomass_mgm3` - Total zooplankton biomass (mg/m³)
#' * `AshFreeBiomass_mgm3` - Ash-free dry weight biomass (mg/m³)
#' * `ZoopAbundance_m3` - Total zooplankton abundance (individuals/m³)
#' * `CopeAbundance_m3` - Copepod abundance (individuals/m³)
#' * `AvgTotalLengthCopepod_mm` - Mean copepod body length (mm)
#' * `OmnivoreCarnivoreCopepodRatio` - Ratio of omnivorous/carnivorous to total copepods
#' * `NoCopepodSpecies_Sample` - Number of copepod species per sample
#' * `ShannonCopepodDiversity` - Shannon diversity index for copepods
#' * `CopepodEvenness` - Pielou's evenness for copepods
#'
#' ### CPR Zooplankton:
#' * `BiomassIndex_mgm3` - Zooplankton biomass index (mg/m³)
#' * `ZoopAbundance_m3`, `CopeAbundance_m3`, `AvgTotalLengthCopepod_mm`,
#'   `OmnivoreCarnivoreCopepodRatio`, `NoCopepodSpecies_Sample`,
#'   `ShannonCopepodDiversity`, `CopepodEvenness` (as above)
#'
#' ### NRS Phytoplankton:
#' * `PhytoBiomassCarbon_pgL` - Phytoplankton carbon biomass (pg/L)
#' * `PhytoAbundance_CellsL` - Phytoplankton abundance (cells/L)
#' * `DiatomDinoflagellateRatio` - Ratio of diatoms to diatoms+dinoflagellates
#' * `AvgCellVol_um3` - Mean cell volume (µm³)
#' * `NoPhytoSpecies_Sample` - Number of phytoplankton species per sample
#' * `ShannonPhytoDiversity` - Shannon diversity index for phytoplankton
#' * `PhytoEvenness` - Pielou's evenness for phytoplankton
#' * Plus diversity metrics for diatoms and dinoflagellates separately
#'
#' ### CPR Phytoplankton:
#' * `PCI` - Phytoplankton Colour Index (visual estimate of phytoplankton biomass)
#' * `PhytoBiomassCarbon_pgm3` - Carbon biomass (pg/m³)
#' * `PhytoAbundance_Cellsm3` - Abundance (cells/m³)
#' * Plus similar metrics as NRS phytoplankton
#'
#' ### NRS Water:
#' * `Secchi_m` - Secchi depth (m)
#' * `MLDtemp_m` - Mixed layer depth from temperature (m)
#' * `MLDsal_m` - Mixed layer depth from salinity (m)
#' * `DCM_m` - Deep chlorophyll maximum depth (m)
#' * `CTDTemperature_degC` - Mean temperature in top 10m (°C)
#' * `CTDSalinity_PSU` - Mean salinity in top 10m (PSU)
#' * `CTDChlaF_mgm3` - Mean chlorophyll fluorescence in top 10m (mg/m³)
#'
#' ### CPR Water:
#' * `PCI` - Phytoplankton Colour Index
#'
#' ## SOTS Data
#' For SOTS (Southern Ocean Time Series), phytoplankton indices are calculated
#' from raw NRS data as they are not provided directly by AODN. Only samples
#' from the LM (Light Microscopy) method at depths <50m are included.
#'
#' @return A dataframe in long format with columns:
#' * For NRS: `StationCode`, `StationName`, `SampleTime_Local`, `Latitude`, `Longitude`
#' * For CPR: `BioRegion`, `SampleTime_Local`, `Latitude`, `Longitude`
#' * Common: `Year_Local`, `Month_Local`, `Parameters` (index name), `Values` (index value)
#'
#' @seealso [pr_filter_data()] to filter the output for specific parameters and stations,
#'   [pr_plot_Trends()], [pr_plot_TimeSeries()], [pr_plot_Climatology()] for visualisation
#'
#' @export
#'
#' @examples
#' # Get NRS phytoplankton indices
#' dat <- pr_get_Indices("NRS", "Phytoplankton")
#' unique(dat$Parameters)
#'
#' # Get CPR zooplankton indices with expanded bioregion boundaries
#' dat <- pr_get_Indices("CPR", "Zooplankton", near_dist_km = 250)
#'
#' # Get water properties from NRS
#' dat <- pr_get_Indices("NRS", "Water")
#'
#' # Filter for specific parameter and stations
#' dat <- pr_get_Indices("NRS", "Zooplankton") %>%
#'   pr_filter_data("Biomass_mgm3", c("MAI", "PHB"))
pr_get_Indices <- function(Survey = "CPR", Type = "Phytoplankton", ...){

  # Input validation
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS', 'CPR', or 'SOTS'."
  )

  assertthat::assert_that(
    Survey %in% c("NRS", "CPR", "SOTS"),
    msg = "'Survey' must be one of 'NRS', 'CPR', or 'SOTS'."
  )

  assertthat::assert_that(
    is.character(Type) && length(Type) == 1,
    msg = "'Type' must be a single character string. Valid options are 'Phytoplankton', 'Zooplankton', or 'Water'."
  )

  if ((Type == "Zooplankton")==TRUE && (Survey == "SOTS") == TRUE) {
    stop("Error: There is no zooplankton data for SOTS, do you mean phytoplankton?")
  }

  Type <- pr_check_type(Type)

  # Validate Type based on Survey
  if (Survey %in% c("NRS", "SOTS")) {
    assertthat::assert_that(
      Type %in% c("Phytoplankton", "Zooplankton", "Water"),
      msg = "For NRS and SOTS surveys, 'Type' must be one of 'Phytoplankton', 'Zooplankton', or 'Water'."
    )
  } else if (Survey == "CPR") {
    assertthat::assert_that(
      Type %in% c("Phytoplankton", "Zooplankton", "Water"),
      msg = "For CPR survey, 'Type' must be one of 'Phytoplankton', 'Zooplankton', or 'Water'."
    )
  }

  if(Type == "Zooplankton" & Survey == "NRS"){
    var_names <- c("Biomass_mgm3", "AshFreeBiomass_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness")
  } else if(Type == "Zooplankton" & Survey == "CPR"){
    var_names <- c("BiomassIndex_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness")
  } else if(Type == "Phytoplankton" & Survey == "CPR"){
    var_names <- c( "PCI", "PhytoBiomassCarbon_pgm3", "PhytoAbundance_Cellsm3", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness")
  } else if(Type == "Phytoplankton" & Survey == "NRS"){
    var_names <- c( "PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness")
  } else if(Type == "Water" & Survey == "NRS"){
    var_names <- c("Secchi_m", "MLDtemp_m", "MLDsal_m", "DCM_m",
                   "CTDTemperature_degC", "CTDSalinity_PSU", "CTDChlaF_mgm3")
  } else if(Type == "Water" & Survey == "CPR"){
    var_names <- c("PCI")
  }

  if(Survey == "CPR"){

    dat <- pr_get_Raw("cpr_derived_indices_data") %>%
      planktonr_dat(Survey = Survey, Type = Type) %>%
      pr_rename() %>%
      pr_add_Bioregions(...) %>%
      pr_apply_Time() %>% #TODO added for consistency but uses etc timezones - do we changes these to the more familiar names or leave? doesn't improve with method = accurate
      dplyr::select(tidyselect::starts_with(c("SampleTime_Local", "Year_Local", "Month_Local",
                                              "BioRegion", "DistanceFromBioregion_m", "tz",
                                              "Latitude", "Longitude", "Sample_ID")),
                    tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      pr_reorder()


    SpInfoZ <- planktonr::pr_get_info(Source = "Zooplankton") %>%
      dplyr::mutate(`Taxon Name` = stringr::str_remove(.data$`Taxon Name`, " [fmji]$"),
                    `Taxon Name` = stringr::str_remove(.data$`Taxon Name`," megalopa"),
                    `Taxon Name` = stringr::str_remove(.data$`Taxon Name`," naupliius"),
                    `Taxon Name` = stringr::str_remove(.data$`Taxon Name`," phyllosoma"),
                    `Taxon Name` = stringr::str_remove(.data$`Taxon Name`," zoea")) %>%
      dplyr::distinct(.data$`Taxon Name`, .keep_all = TRUE) %>%
      dplyr::select(c(TaxonName = "Taxon Name", FunctionalGroup = "Functional Group", "Genus", "Species",
                      "Diet", AphiaID = "WoRMS AphiaID", "Length (mm)"))



    if (Type == "Zooplankton"){
      main_vars <- c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz",
                     "Latitude", "Longitude", "SampleTime_UTC", "SampleVolume_m3",
                     "BioRegion", "Sample_ID", "TotalCount", "PCI")


      grp <- setdiff(main_vars, "PCI")

      dat <- cpr_AAD %>%
        tidyr::pivot_longer(-tidyselect::all_of(main_vars), names_to = "TaxonName", values_to = "Count") %>%
        # dplyr::left_join(trophy, by = "TaxonName") %>%
        dplyr::filter(.data$Count > 0) %>%
        dplyr::left_join(SpInfoZ, by = c("TaxonName" = "TaxonName")) %>%  # join the species info.
        dplyr::group_by(dplyr::across(tidyselect::all_of(grp))) %>%
        dplyr::summarise(PCI = mean(.data$PCI, na.rm = TRUE),
                         BiomassIndex_mgm3 = NA,
                         ZoopAbundance_m3 = sum(.data$Count) / (mean(.data$SampleVolume_m3)),
                         CopeAbundance_m3 = sum(.data$Count[.data$FunctionalGroup == "Copepod"], na.rm = TRUE) / sum(.data$SampleVolume_m3, na.rm = TRUE),
                         AvgTotalLengthCopepod_mm = mean(.data$`Length (mm)`[.data$FunctionalGroup == "Copepod"], na.rm = TRUE),
                         NoCopepodSpecies_Sample = length(.data$FunctionalGroup == "Copepod"),
                         OmnivoreCarnivoreCopepodRatio =
                           sum(.data$Count[.data$FunctionalGroup == "Copepod" & .data$Diet == "Omnivore"], na.rm = TRUE) / # Number of omnivores to
                           sum(.data$Count[.data$FunctionalGroup == "Copepod" & .data$Diet == "Carnivore"], na.rm = TRUE), # Number of carnivores
                         ShannonCopepodDiversity = vegan::diversity(
                           x = .data$Count[.data$FunctionalGroup == "Copepod"] /
                             .data$SampleVolume_m3[.data$FunctionalGroup == "Copepod"],
                           index = "shannon"),
                         CopepodEvenness = .data$ShannonCopepodDiversity/log10(.data$NoCopepodSpecies_Sample)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ dplyr::na_if(.x , y = NaN))) %>%
        tidyr::pivot_longer(-tidyselect::any_of(grp), values_to = "Values", names_to = "Parameters") %>%
        planktonr::planktonr_dat("Zooplankton", "CPR") %>%
        planktonr::pr_remove_outliers(2) %>%
        droplevels() %>%
        dplyr::select(colnames(dat)) %>%
        dplyr::bind_rows(dat, .) %>%
        pr_reorder()
    }


  } else if (Survey == "NRS"){

    dat <- pr_get_Raw("nrs_derived_indices_data") %>%
      planktonr_dat(Survey = Survey, Type = Type) %>%
      pr_rename() %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      pr_add_StationCode() %>%
      pr_apply_Time() %>%
      dplyr::select("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude",
                    "StationName", "StationCode", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      pr_reorder()

  } else if (Survey == "SOTS"){
    # SOTS Indices not available from AODN so calculated here

    trophy <- planktonr::pr_get_info(Source = "Phytoplankton") %>% # for information used in estimating Indices as per NRS data
      dplyr::select(TaxonName = "Taxon Name",
                    Trophy = "Functional Type",
                    Carbon = "Cell Carbon (pgN cell-1)",
                    CellBioV = "Cell BioVolume (um3)",
                    FunctionalGroup = "Functional Group") %>%
      dplyr::mutate(genus = dplyr::case_when(stringr::word(.data$TaxonName, 1) == "cf." ~ stringr::word(.data$TaxonName, 2),
                                             TRUE ~ stringr::word(.data$TaxonName, 1)),
                    species = dplyr::case_when(stringr::word(.data$TaxonName, 2) == "cf." ~ "spp.",
                                               stringr::word(.data$TaxonName, 1) == "cf." ~ "spp.",
                                               grepl("\\(|\\/|diatom|dino|\\-|group", .data$TaxonName) ~ "spp.",
                                               TRUE ~ stringr::word(.data$TaxonName, 2)))

    main_vars <- c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude", "StationName", "StationCode", "Method", "SampleDepth_m")

    dat <- pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance", Subset = "raw") %>%
      dplyr::filter(grepl("SOTS", .data$StationCode),
                    .data$Method == "LM", # only use LM at this stage
                    .data$SampleDepth_m < 50) %>% # remove deep samples taken at CTD depths
      tidyr::pivot_longer(-c("Project":"Method"), names_to = "TaxonName", values_to = "abund") %>%
      dplyr::select(tidyselect::any_of(main_vars), "TaxonName", "abund") %>%
      dplyr::left_join(trophy, by = "TaxonName") %>%
      dplyr::filter(.data$abund > 0) %>%
      dplyr::mutate(TaxonName = paste0(.data$genus, " ", .data$species),
                    tz = "Australia/Hobart",
                    StationName = "Southern Ocean Time Series",
                    StationCode = "SOTS") %>%
      dplyr::group_by(dplyr::across(tidyselect::any_of(main_vars)), .data$TaxonName, .data$FunctionalGroup, .data$CellBioV, .data$Carbon) %>%
      dplyr::summarise(abund = sum(.data$abund, na.rm = TRUE), ## add up all occurrences of spp. within one sample
                       .groups = "drop") %>%
      dplyr::group_by(dplyr::across(tidyselect::any_of(main_vars))) %>%
      dplyr::summarise(AvgCellVol_um3 = mean(.data$CellBioV*.data$abund/sum(.data$abund), na.rm = TRUE),
                       DiatomDinoflagellateRatio = sum(.data$abund[grepl("iatom", .data$FunctionalGroup)])/sum(.data$abund[grepl("iatom|Dinof", .data$FunctionalGroup)]),
                       NoPhytoSpecies_Sample = length(.data$abund[!grepl("NA|spp", .data$TaxonName)]),
                       NoDiatomSpecies_Sample = length(.data$abund[grepl("iatom", .data$FunctionalGroup) & !grepl("NA|spp", .data$TaxonName)]),
                       NoDinoSpecies_Sample = length(.data$abund[.data$FunctionalGroup == "Dinoflagellate" & !grepl("NA|spp", .data$TaxonName)]),
                       PhytoAbundance_CellsL = sum(.data$abund, na.rm = TRUE),
                       PhytoBiomassCarbon_pgL = sum(.data$abund * .data$Carbon, na.rm = TRUE),
                       ShannonPhytoDiversity = vegan::diversity(.data$abund[!grepl("NA|spp", .data$TaxonName)], index = "shannon"),
                       ShannonDiatomDiversity = vegan::diversity(.data$abund[grepl("iatom", .data$FunctionalGroup) & !grepl("NA|spp", .data$TaxonName)], index = "shannon"),
                       ShannonDinoDiversity = vegan::diversity(.data$abund[grepl("Dinof", .data$FunctionalGroup) & !grepl("NA|spp", .data$TaxonName)], index = "shannon"),
                       PhytoEvenness = .data$ShannonPhytoDiversity/log10(.data$NoPhytoSpecies_Sample),
                       DiatomEvenness = .data$ShannonDiatomDiversity/log10(.data$NoDiatomSpecies_Sample),
                       DinoflagellateEvenness = .data$ShannonDinoDiversity/log10(.data$NoDinoSpecies_Sample),
                       .groups = "drop") %>%
      tidyr::pivot_longer(-tidyselect::any_of(main_vars), values_to = "Values", names_to = "Parameters") %>%
      planktonr::pr_remove_outliers(2) %>%
      droplevels() %>%
      planktonr::planktonr_dat("Phytoplankton", "SOTS")

  }

  return(dat)
}


#' Filter data for plotting functions
#'
#' Filter plankton data by parameter and location
#'
#' Convenience function to filter index data to specific parameters and locations
#' before plotting or analysis. Automatically detects whether data uses station
#' codes (NRS) or bioregions (CPR) and filters accordingly.
#'
#' @param dat A dataframe from [pr_get_Indices()], [pr_get_EOVs()], or similar
#'   functions
#' @param Parameter Character string or vector of parameter names to retain.
#'   Common parameters include:
#'   * **Biomass**: `"Biomass_mgm3"`, `"BiomassIndex_mgm3"`, `"PhytoBiomassCarbon_pgL"`
#'   * **Abundance**: `"Abundance_m3"`, `"AbundanceIndex_Samplekm"`, `"TotalCopepodAbundance_m3"`
#'   * **Diversity**: `"ShannonDiversity"`, `"TotalTaxa_Sample"`, `"Richness"`
#' @param StationRegion Character string or vector of locations to retain:
#'   * **For NRS data**: Station codes like `"NSI"`, `"PHB"`, `"MAI"`, `"ROT"`,
#'     `"YON"`, `"DAR"`, `"KAI"`, `"ESP"`, `"NIN"`
#'   * **For CPR data**: Bioregions like `"Temperate East"`, `"South-east"`,
#'     `"South-west"`, `"North-west"`, `"Coral Sea"`
#'
#' @details
#' This function streamlines the common workflow of filtering data before creating
#' plots or running analyses. It:
#' * Detects data type automatically (NRS vs CPR)
#' * Filters on `Parameters` column to match `Parameter` argument
#' * Filters on `StationCode` (NRS) or `BioRegion` (CPR) to match `StationRegion` argument
#' * Validates inputs to catch common errors
#'
#' ## When to Use
#' Use this function when:
#' * Creating plots for specific stations or regions
#' * Comparing multiple parameters at the same location(s)
#' * Preparing data for statistical analysis on a subset of locations
#'
#' ## Multiple Selections
#' Both `Parameter` and `StationRegion` accept vectors, enabling:
#' * Multiple parameters at one station: `Parameter = c("Biomass_mgm3", "Abundance_m3"), StationRegion = "NSI"`
#' * One parameter at multiple stations: `Parameter = "Biomass_mgm3", StationRegion = c("NSI", "PHB", "MAI")`
#' * Multiple parameters at multiple stations: vectors for both arguments
#'
#' @return A filtered dataframe maintaining the same structure as input
#'
#' @seealso
#' * [pr_get_Indices()] for loading data before filtering
#' * [pr_plot_TimeSeries()], [pr_plot_Trends()], [pr_plot_Climatology()] for plotting filtered data
#'
#' @export
#'
#' @examples
#' # Filter CPR data to biomass in two bioregions
#' dat <- pr_get_Indices("CPR", "Zooplankton") %>%
#'   pr_filter_data("BiomassIndex_mgm3", c("North", "South-west"))
#'
#' # Filter NRS data to phytoplankton carbon at two stations
#' dat <- pr_get_Indices("NRS", "Phytoplankton") %>%
#'   pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB"))
#'
#' # Multiple parameters at one station
#' dat <- pr_get_Indices("NRS", "Zooplankton") %>%
#'   pr_filter_data(c("Biomass_mgm3", "Abundance_m3", "ShannonDiversity"), "MAI")
pr_filter_data <- function(dat, Parameter = "Biomass_mgm3", StationRegion = "NSI"){

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame."
  )

  assertthat::assert_that(
    inherits(dat, "planktonr_dat"),
    msg = "'dat' must be a planktonr_dat object. Use pr_get_Indices() or similar functions to create the data."
  )

  assertthat::assert_that(
    nrow(dat) > 0,
    msg = "The data frame 'dat' is empty. Check your data source or filtering criteria."
  )

  assertthat::assert_that(
    is.character(Parameter),
    msg = "'Parameter' must be a character string or character vector specifying which parameter(s) to filter."
  )

  assertthat::assert_that(
    is.character(StationRegion),
    msg = "'StationRegion' must be a character string or character vector specifying which station(s) or region(s) to filter."
  )

  if("StationName" %in% colnames(dat)) {
    dat <- dat %>%
      dplyr::filter(.data$Parameters %in% Parameter,
                    .data$StationCode %in% StationRegion)
  } else {
    dat <- dat %>%
      dplyr::filter(.data$Parameters %in% Parameter,
                    .data$BioRegion %in% StationRegion)
  }
}


#' To produce the climatology for plotting
#'
#' @param dat output of pr_get_Indices
#' @param x A string of Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_Climatology functions
#' @export
#'
#' @examples
#' dat <- data.frame(Month = rep(1:12,10), StationCode = "NSI", Values = runif(120, min=0, max=10))
#' pr_make_climatology(dat, "Month")
#' @importFrom stats sd
#' @importFrom rlang .data
pr_make_climatology <- function(dat, x){

  dat_climate <- dat %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .by = tidyselect::all_of(c(x, "StationCode")))
  return(dat_climate)
}
