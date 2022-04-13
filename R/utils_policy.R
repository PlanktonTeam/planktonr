
#' Get Policy data set
#'
#' @param Survey "NRS" or "CPR"
#'
#' @return A dataframe with policy data
#' @export
#'
#' @examples
#' dat <- pr_get_pol("NRS")
pr_get_pol <- function(Survey = "NRS"){

  if(Survey == "CPR") {

    var_names <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                    "ShannonCopepodDiversity", "ShannonPhytoDiversity")

    Polr <- dat <- pr_get_raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_SampleDate() %>%
      pr_add_Bioregions()

    Pol <- Polr %>%
      dplyr::select(.data$SampleDate_UTC, .data$Year, .data$Month, .data$BioRegion,
                    tidyselect::all_of(var_names)) %>%
      dplyr::filter(!.data$BioRegion %in% c("North", "North-west")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "parameters")

    means <- Polr %>%
      dplyr::select(.data$BioRegion, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "parameters") %>%
      dplyr::group_by(.data$BioRegion, .data$parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("BioRegion", "parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd)

  } else if (Survey == "NRS"){

    var_names <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL",
                   "CTDTemperature_degC", "ShannonCopepodDiversity",
                   "ShannonPhytoDiversity", "CTDSalinity_PSU", "PigmentChla_mgm3")

    Polr <- pr_get_raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      dplyr::mutate(Month = lubridate::month(.data$SampleDate_Local),
                    Year = lubridate::year(.data$SampleDate_Local))

    Pol <- Polr %>%
      dplyr::select(.data$SampleDate_Local, .data$Year, .data$Month, .data$StationName,
                    .data$StationCode, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "parameters")

    means <- Polr %>%
      dplyr::select(.data$StationName, tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "parameters") %>%
      dplyr::group_by(.data$StationName, .data$parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("StationName", "parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd)

  }
}

#' Get policy information
#'
#' @param Survey "NRS" or "CPR"
#'
#' @return A dataframe with policy information
#' @export
#'
#' @examples
#' df <- pr_get_polInfo("NRS")
pr_get_polInfo <- function(Survey = "NRS"){

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
      dplyr::mutate(Latitude = .data$STARTLATITUDE,
                    Longitude = .data$ENDLONGITUDE) %>%
      pr_add_Bioregions() %>%
      dplyr::select(.data$BioRegion, .data$STARTSAMPLEDATEUTC, .data$MILES) %>%
      dplyr::mutate(Features = dplyr::case_when(.data$BioRegion %in% c("South-east") ~ "narrow shelf intensifying currents, eddies and upwellings with low nutrient and primary productivity",
                                                .data$BioRegion %in% c("South-west") ~ "temperate and subtropical habitats influenced by the nutrient deplete Leeuwin Current.",
                                                .data$BioRegion %in% c("Temperate East") ~ "temperate and subtropical habitats influenced by the East Australian Current and its eddies.",
                                                .data$BioRegion %in% c("Temperate East") ~ "Western Pacific Warm Pool water mass with low surface nutrient levels"))
  }
}
