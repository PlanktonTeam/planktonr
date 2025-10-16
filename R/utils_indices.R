#' Access data for timeseries and climatology plots
#'
#' @param Survey CPR or NRS, defaults to NRS
#' @param Type Phyto, zoo or physical water props, defaults to phyto
#' @param ... variables to be passed to pr_add_Bioregions. At the moment it only supports `near_dist_km` which is the distance (km) around each bioregion to pad the allocation of points.
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "Phytoplankton")
#' df <- pr_get_Indices("NRS", "Zooplankton")
#' df <- pr_get_Indices("NRS", "Water")
#' df <- pr_get_Indices("CPR", "Phytoplankton", near_dist_km = 250)
#' df <- pr_get_Indices("CPR", "Zooplankton")
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

    trophy <- planktonr::pr_get_PlanktonInfo(Type = "Phytoplankton") %>% # for information used in estimating Indices as per NRS data
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

    dat <- planktonr::pr_get_NRSData(Type = "phytoplankton", Variable = "abundance", Subset = "raw") %>%
      dplyr::filter(grepl("SOTS", .data$StationCode),
                    .data$Method == "LM", # only use LM at this stage
                    .data$SampleDepth_m < 50) %>% # remove deep samples taken at CTD depths
      tidyr::pivot_longer(-c(.data$Project:.data$Method), names_to = "TaxonName", values_to = "abund") %>%
      dplyr::select(tidyselect::any_of(main_vars), .data$TaxonName, .data$abund) %>%
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
#' @param df dataframe from pr_get_indices
#' @param Parameter parameter(s) to be filtered on
#' @param StationRegion StationCode(s) or BioRegion(s) to be filtered on
#'
#' @return prefiltered data frame for pr_plot_trends, ts, climate
#' @export
#'
#' @examples
#' df <- pr_get_Indices("CPR", "Zooplankton") %>%
#'       pr_filter_data("BiomassIndex_mgm3", c("North", "South-west"))
#' df <- pr_get_Indices("NRS", "Phytoplankton") %>%
#'       pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB"))
pr_filter_data <- function(df, Parameter = "Biomass_mgm3", StationRegion = "NSI"){

  # Input validation
  assertthat::assert_that(
    is.data.frame(df),
    msg = "'df' must be a data frame."
  )

  assertthat::assert_that(
    inherits(df, "planktonr_dat"),
    msg = "'df' must be a planktonr_dat object. Use pr_get_Indices() or similar functions to create the data."
  )

  assertthat::assert_that(
    nrow(df) > 0,
    msg = "The data frame 'df' is empty. Check your data source or filtering criteria."
  )

  assertthat::assert_that(
    is.character(Parameter),
    msg = "'Parameter' must be a character string or character vector specifying which parameter(s) to filter."
  )

  assertthat::assert_that(
    is.character(StationRegion),
    msg = "'StationRegion' must be a character string or character vector specifying which station(s) or region(s) to filter."
  )

  if("StationName" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(.data$Parameters %in% Parameter,
                    .data$StationCode %in% StationRegion)
  } else {
    df <- df %>%
      dplyr::filter(.data$Parameters %in% Parameter,
                    .data$BioRegion %in% StationRegion)
  }
}


#' To produce the climatology for plotting
#'
#' @param df output of pr_get_Indices
#' @param x A string of Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_Climatology functions
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), StationCode = "NSI", Values = runif(120, min=0, max=10))
#' pr_make_climatology(df, "Month")
#' @importFrom stats sd
#' @importFrom rlang .data
pr_make_climatology <- function(df, x){

  df_climate <- df %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .by = tidyselect::all_of(c(x, "StationCode")))
  return(df_climate)
}
