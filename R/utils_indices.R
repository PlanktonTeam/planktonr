#' Access data for timeseries and climatology plots
#'
#' @param Survey CPR or NRS, defaults to NRS
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_indices("NRS", "P")
#' df <- pr_get_indices("NRS", "Z")
#' df <- pr_get_indices("CPR", "P")
#' df <- pr_get_indices("CPR", "Z")
pr_get_indices <- function(Survey = "CPR", Type = "P"){

  if(Type == "Z" & Survey == "NRS"){
    var_names <- c("Biomass_mgm3", "AshFreeBiomass_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness")
  } else if(Type == "Z" & Survey == "CPR"){
    var_names <- c("BiomassIndex_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3", "AvgTotalLengthCopepod_mm",
                   "OmnivoreCarnivoreCopepodRatio", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity", "CopepodEvenness")
  } else if(Type == "P" & Survey == "CPR"){
    var_names <- c( "PhytoBiomassCarbon_pgm3", "PhytoAbundance_Cellsm3", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness")
  } else if(Type == "P" & Survey == "NRS"){
    var_names <- c( "PhytoBiomassCarbon_pgL", "PhytoAbund_CellsL", "DiatomDinoflagellateRatio",
                    "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness",
                    "NoDiatomSpecies_Sample", "ShannonDiatomDiversity", "DiatomEvenness", "NoDinoSpecies_Sample",
                    "ShannonDinoDiversity", "DinoflagellateEvenness")
  }


  if(Survey == "CPR"){

    dat <- pr_get_raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_SampleDate() %>%
      pr_add_Bioregions() %>%
      dplyr::select(.data$SampleDate_UTC, .data$Year, .data$Month, .data$Day, .data$BioRegion,
                    tidyselect::all_of(var_names)) %>%
      dplyr::mutate(SampleDate_UTC = lubridate::round_date(.data$SampleDate_UTC, "month"), #TODO Check with Claire about this rounding
                    YearMon = paste(.data$Year, .data$Month)) %>% #TODO this step can be improved when nesting supports data pronouns
      tidyr::complete(.data$BioRegion, .data$YearMon) %>%
      dplyr::mutate(Year = as.numeric(stringr::str_sub(.data$YearMon, 1, 4)), #TODO Not sure why Year/Month is redefined here
                    Month = as.numeric(stringr::str_sub(.data$YearMon, -2, -1))) %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "parameters") %>%
      dplyr::group_by(.data$SampleDate_UTC, .data$Year, .data$Month, .data$BioRegion, .data$parameters) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = "drop") %>%
      pr_reorder() %>%
      dplyr::filter(!is.na(.data$BioRegion), !.data$BioRegion %in% c("North", "North-west")) %>%
      droplevels()

    return(dat)

  } else if(Survey == "NRS"){

    dat <- pr_get_raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      dplyr::mutate(Month = lubridate::month(.data$SampleDate_Local),
                    Year = lubridate::year(.data$SampleDate_Local)) %>%
      tidyr::complete(.data$Year, .data$StationCode) %>% #TODO Nesting doesn't support data pronouns at this time
      dplyr::select(.data$Year, .data$Month, .data$SampleDate_Local, .data$StationName, .data$StationCode,
                    tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "parameters") %>%
      pr_reorder()

    return(dat)
  }
}

#' To produce the climatology for plotting
#'
#' @param df output of pr_get_indices
#' @param x Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_climate functions
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), StationCode = 'NSI', Values = runif(120, min=0, max=10))
#' pr_make_climatology(df, Month)
#' @importFrom stats sd
#' @importFrom rlang .data
pr_make_climatology <- function(df, x){
  x <- dplyr::enquo(arg = x)
  df_climate <- df %>% dplyr::filter(!!x != "NA") %>% # need to drop NA from month, added to dataset by complete(Year, StationCode)
    dplyr::group_by(!!x, .data$StationCode) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")
  return(df_climate)
}
