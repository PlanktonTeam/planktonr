#' Access data for timeseries and climatology plots
#'
#' @param Survey CPR or NRS, defaults to NRS
#' @param Type Phyto, zoo or physical water props, defaults to phyto
#' @param ... to allow use of join when used within another function
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "P")
#' df <- pr_get_Indices("NRS", "Z")
#' df <- pr_get_Indices("NRS", "W")
#' df <- pr_get_Indices("CPR", "P", join = "st_nearest_feature")
#' df <- pr_get_Indices("CPR", "Z")
pr_get_Indices <- function(Survey = "CPR", Type = "P", ...){

  if(Type == "Z" & Survey == "NRS"){
    var_names <- c("Biomass_mgm3", "AshFreeBiomass_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness")
  } else if(Type == "Z" & Survey == "CPR"){
    var_names <- c("BiomassIndex_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness", "DistanceFromBioregion_m")
  } else if(Type == "P" & Survey == "CPR"){
    var_names <- c( "PhytoBiomassCarbon_pgm3", "PhytoAbundance_Cellsm3", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness", "DistanceFromBioregion_m")
  } else if(Type == "P" & Survey == "NRS"){
    var_names <- c( "PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness")
  } else if(Type == "W" & Survey == "NRS"){
    var_names <- c("Secchi_m", "MLDtemp_m", "MLDsal_m", "DCM_m",
                   "CTDTemperature_degC", "CTDSalinity_PSU", "CTDChlaF_mgm3")
  } else if(Type == "W" & Survey == "CPR"){
    var_names <- c("PCI")
  }

  if(Survey == "CPR"){

    dat <- pr_get_Raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_Bioregions(...) %>%
      pr_apply_Time() %>% #TODO added for consistency but uses etc timezones - do we changes these to the more familiar names or leave? doesn't improve with method = accurate
      dplyr::select(.data$Year_Local, .data$Month_Local, .data$SampleTime_Local, .data$tz, .data$Latitude, .data$Longitude, .data$BioRegion, tidyselect::any_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      pr_reorder()



    return(dat)

  } else if(Survey == "NRS"){

    dat <- pr_get_Raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      pr_apply_Time() %>%
      dplyr::select(.data$Year_Local, .data$Month_Local, .data$SampleTime_Local, .data$tz, .data$Latitude, .data$Longitude,
                    .data$StationName, .data$StationCode, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      pr_reorder()

    return(dat)
  }
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
#' df <- pr_get_Indices("CPR", "Z") %>%
#'       pr_filter_data("BiomassIndex_mgm3", c("North", "South-west"))
#' df <- pr_get_Indices("NRS", "P") %>%
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
#' @param x Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_Climatology functions
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), StationCode = 'NSI', Values = runif(120, min=0, max=10))
#' pr_make_climatology(df, Month)
#' @importFrom stats sd
#' @importFrom rlang .data
pr_make_climatology <- function(df, x){
  x <- dplyr::enquo(arg = x)
  df_climate <- df %>% dplyr::filter(!!x != "NA") %>% # TODO Can I remove this now I have removed complete?  # need to drop NA from month, added to dataset by complete(Year, StationCode)
    dplyr::group_by(!!x, .data$StationCode) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")
  return(df_climate)
}
