
#' Get Policy data set
#'
#' @param Survey "NRS" or "CPR"
#'
#' @return
#' @export
#'
#' @examples
#' dat <- pr_get_pol("NRS")
pr_get_pol <- function(Survey = "NRS"){
  if(Survey == "CPR"){
    file <- "CPR_Indices.csv"
  } else {
    file <- "NRS_Indices.csv"
  }

  Polr <- readr::read_csv(system.file("extdata", file, package = "planktonr", mustWork = TRUE),
                          na = c("NA", ""),
                          show_col_types = FALSE)

  if(Survey == "CPR") {

    Pol <- Polr %>%
      dplyr::select(.data$SampleDateUTC, .data$Year, .data$Month, .data$BioRegion,
                    .data$Biomass_mgm3, .data$PhytoBiomassCarbon_pgm3,
                    .data$ShannonCopepodDiversityCPR, .data$ShannonPhytoDiversitycpr) %>%
      dplyr::filter(!.data$BioRegion %in% c("North", "North-west")) %>%
      tidyr::pivot_longer(-c(.data$SampleDateUTC:.data$BioRegion), values_to = "Values", names_to = "parameters")

    means <- Polr %>%
      dplyr::select(.data$BioRegion, .data$Biomass_mgm3, .data$PhytoBiomassCarbon_pgm3,
                    .data$ShannonCopepodDiversityCPR, .data$ShannonPhytoDiversitycpr) %>%
      tidyr::pivot_longer(-c(.data$BioRegion), values_to = "Values", names_to = "parameters") %>%
      dplyr::group_by(.data$BioRegion, .data$parameters) %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .groups = "drop")

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("BioRegion", "parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd)

  } else {

    Pol <- Polr %>%
      dplyr::select(.data$SampleDateLocal, .data$Year, .data$Month, .data$StationName,
                    .data$StationCode, .data$Biomass_mgm3, .data$PhytoBiomassCarbon_pgL,
                    .data$CTDTemperature_degC, .data$ShannonCopepodDiversity,
                    .data$ShannonPhytoDiversity, .data$CTDSalinity_psu, .data$PigmentChla_mgm3) %>%
      tidyr::pivot_longer(-c(.data$SampleDateLocal:.data$StationCode), values_to = "Values", names_to = "parameters") %>%
      dplyr::mutate(SampleDateLocal = strptime(as.POSIXct(.data$SampleDateLocal), "%Y-%m-%d"))

    means <- Polr %>%
      dplyr::select(.data$StationName, .data$Biomass_mgm3, .data$PhytoBiomassCarbon_pgL,
                    .data$CTDTemperature_degC, .data$ShannonCopepodDiversity,
                    .data$ShannonPhytoDiversity, .data$CTDSalinity_psu, .data$PigmentChla_mgm3) %>%
      tidyr::pivot_longer(-c(.data$StationName), values_to = "Values", names_to = "parameters") %>%
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
#' @return
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
