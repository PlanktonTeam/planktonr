
#' Get Policy data set
#' @param ... to allow use of join when used within another function
#' @param Survey "NRS" or "CPR" or "LTM"
#'
#' @return A dataframe with policy data
#' @export
#'
#' @examples
#' df <- pr_get_PolicyData("CPR", join = 'st_nearest_feature')
pr_get_PolicyData <- function(Survey = "NRS", ...){

  if(Survey == "CPR") {

    var_names <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                    "ShannonCopepodDiversity", "ShannonPhytoDiversity")

    Polr <- pr_get_Raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_Bioregions(...)

    Pol <- Polr %>%
      dplyr::select(tidyselect::starts_with(c("SampleTime_Local", "Year_Local", "Month_Local", "BioRegion", "DistanceFromBioregion_m",
                    tidyselect::all_of(var_names)))) %>%
      dplyr::filter(!.data$BioRegion %in% c("North", "North-west")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select(.data$BioRegion, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::group_by(.data$BioRegion, .data$Parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("BioRegion", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = 'CPR')

  } else if (Survey == "NRS"){

    var_names <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL",
                   "CTDTemperature_degC", "ShannonCopepodDiversity",
                   "ShannonPhytoDiversity", "CTDSalinity_PSU", "PigmentChla_mgm3")

    Polr <- pr_get_Raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local),
                    Year_Local = lubridate::year(.data$SampleTime_Local))

    Pol <- Polr %>%
      dplyr::select(.data$SampleTime_Local, .data$Year_Local, .data$Month_Local, .data$StationName,
                    .data$StationCode, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select(.data$StationName, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::group_by(.data$StationName, .data$Parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = 'NRS')

  } else if (Survey == "LTM"){

    LTnuts <- pr_get_LTnuts()

    means <- LTnuts %>%
      dplyr::select(.data$StationName, .data$Parameters, .data$Values) %>%
      dplyr::group_by(.data$StationName, .data$Parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- LTnuts %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - .data$means)/.data$sd,
                    Survey = 'LTM')

  }
}

#' Get policy information
#' @param ... to allow use of join when used within another function
#' @param Survey "NRS" or "CPR"
#'
#' @return A dataframe with policy information
#' @export
#'
#' @examples
#' df <- pr_get_PolicyInfo("CPR")
pr_get_PolicyInfo <- function(Survey = "NRS", ...){

  if(Survey == "NRS"){

    NRSinfo <- pr_get_NRSStation() %>%
      dplyr::mutate(Region = dplyr::case_when(.data$StationCode %in% c("DAR") ~ "Tropical North",
                                              .data$StationCode %in% c("YON") ~ "GBR Lagoon",
                                              .data$StationCode %in% c("NSI", "PHB", "MAI") ~ "South East",
                                              .data$StationCode %in% c("KAI") ~ "South Central",
                                              .data$StationCode %in% c("ROT", "ESP", "NIN") ~ "South West"),
                    Features = dplyr::case_when(.data$StationCode %in% c("DAR") ~ "broad, shallow shelf seas with strong tidal influence and tropical neritic communities.",
                                                .data$StationCode %in% c("YON") ~ "shallow water influenced by the EAC and Hiri currents and is floristically distinct.",
                                                .data$StationCode %in% c("NSI", "PHB", "MAI") ~ "very narrow shelf influenced by the EAC and its eddies with temperate neritic communities",
                                                .data$StationCode %in% c("KAI") ~ "upwelling systems and the Leeuwin and Flinders currents and covers the GAB and SA Gulf.",
                                                .data$StationCode %in% c("ROT", "ESP", "NIN") ~ "narrow shelf influenced by the Leeuwin Current with tropical oeanic communities"),
                    now = dplyr::case_when(.data$StationCode %in% c("DAR", "YON", "NSI", "PHB", "MAI", "KAI", "ROT") ~ "and is ongoing",
                                           .data$StationCode %in% c("ESP", "NIN") ~ "and concluded in March 2013")) %>%
      dplyr::select(-c(.data$ProjectName, .data$StationCode, .data$IMCRA))

  } else {

    CPRinfo <- pr_get_CPRTrips() %>%
      pr_add_Bioregions(...) %>%
      dplyr::group_by(.data$BioRegion) %>%
      dplyr::summarise(SampleStartDate = as.Date(min(.data$SampleTime_UTC)),
                       Miles = dplyr::n() * 4 * 5) %>%
      dplyr::mutate(Features = dplyr::case_when(.data$BioRegion %in% c("South-east") ~ "narrow shelf intensifying currents, eddies and upwellings with low nutrient and primary productivity",
                                                .data$BioRegion %in% c("South-west") ~ "temperate and subtropical habitats influenced by the nutrient deplete Leeuwin Current.",
                                                .data$BioRegion %in% c("Temperate East") ~ "temperate and subtropical habitats influenced by the East Australian Current and its eddies.",
                                                .data$BioRegion %in% c("Coral Sea") ~ "Western Pacific Warm Pool water mass with low surface nutrient levels"))
  }
}
