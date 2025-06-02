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

  Type <- pr_check_type(Type)

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
      pr_rename() %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      pr_add_StationCode() %>%
      pr_apply_Time() %>%
      dplyr::select("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude",
                    "StationName", "StationCode", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      pr_reorder()

    # dat <- dat %>%
    #   dplyr::filter(.data$StationCode != "VBM") # TODO Temporarily remove VBM - Not enough data to run analyses

  }

  # Convert to planktonr class
  dat <- planktonr_dat(dat, type = Type, survey = Survey, variable = NULL, subset = NULL)

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
