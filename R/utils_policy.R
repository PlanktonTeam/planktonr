
#' Get Essential Ocean Variables
#' @param Survey "NRS" or "CPR" or "LTM"
#' @param ... to allow use of join when used within another function
#'
#' @return A dataframe with policy data
#' @export
#'
#' @examples
#' df <- pr_get_EOVs("CPR")
pr_get_EOVs <- function(Survey = "NRS", ...){

  if(Survey == "CPR") {

    var_names <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                    "ShannonCopepodDiversity", "ShannonPhytoDiversity", "SST", "chl_oc3")

    Polr <- pr_get_Raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_Bioregions(...)

    polSat <- pr_get_SatData("CPR") %>%
      dplyr::select("Sample_ID", "SST", "chl_oc3")

    Polr <-  Polr %>%
      dplyr::left_join(polSat, by = "Sample_ID")

    Pol <- Polr %>%
      dplyr::select(tidyselect::starts_with(c("SampleTime_Local", "Year_Local", "Month_Local", "BioRegion", "DistanceFromBioregion_m")),
                    tidyselect::all_of(var_names)) %>%
      dplyr::filter(!.data$BioRegion %in% c("North", "North-west")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select("BioRegion", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("BioRegion", "Parameters")))

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("BioRegion", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = "CPR") %>%
      planktonr::pr_reorder()

  } else if (Survey == "NRS"){

    var_names <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL",
                   "CTDTemperature_degC", "ShannonCopepodDiversity",
                   "ShannonPhytoDiversity", "Salinity", "PigmentChla_mgm3",
                   "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL", "Silicate_umolL",
                   "Phosphate_umolL", "Oxygen_umolL")

    Polr <- pr_get_Raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local),
                    Year_Local = lubridate::year(.data$SampleTime_Local))

    Pol <- Polr %>%
      dplyr::select("SampleTime_Local", "Year_Local", "Month_Local", "StationName",
                    "StationCode", tidyselect::all_of(var_names)) %>%
      dplyr::filter(!.data$StationName %in% c("Port Hacking 4")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select("StationName", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationName", "Parameters")))

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = "NRS") %>%
      planktonr::pr_reorder()


    # Pol <- Pol %>%
    #   dplyr::filter(.data$StationCode != "VBM") # TODO Temporarily remove VBM - Not enough data to run analyses

  } else if (Survey == "LTM"){

    all_vars <- c("StationName", "StationCode", "SampleTime_Local", "Month_Local", "Year_Local", "Parameters")

    LTnuts <- pr_get_LTnuts() %>%
      dplyr::filter(.data$SampleDepth_m < 11) %>%
      dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                       .by = all_vars) %>%
      dplyr::rename(Values = mean)

    means <- LTnuts %>%
      dplyr::select("StationName", "Parameters", "Values") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationName", "Parameters")))

    Pol <- LTnuts %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - .data$means)/.data$sd,
                    Survey = 'LTM') %>%
      planktonr::pr_reorder()

  }

  Pol <- pr_planktonr_class(Pol, type = "EOV", survey = Survey, variable = NULL)

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
                                              .data$StationCode %in% c("KAI", "VBM") ~ "South Central",
                                              .data$StationCode %in% c("ROT", "ESP", "NIN") ~ "South West"),
                    Features = dplyr::case_when(.data$StationCode %in% c("DAR") ~ "broad, shallow shelf seas with strong tidal influence and tropical neritic communities",
                                                .data$StationCode %in% c("YON") ~ "shallow water influenced by the EAC and Hiri currents and is floristically distinct",
                                                .data$StationCode %in% c("NSI", "PHB", "MAI") ~ "very narrow shelf influenced by the EAC and its eddies with temperate neritic communities",
                                                .data$StationCode %in% c("KAI") ~ "upwelling systems and the Leeuwin and Flinders currents and covers the GAB and SA Gulf",
                                                .data$StationCode %in% c("VBM") ~ "shelf waters between Cape Jaffa in South Australia and Cape Otway in Victoria and hosts a strong seasonal wind-driven coastal upwelling system",
                                                .data$StationCode %in% c("ROT", "ESP", "NIN") ~ "narrow shelf influenced by the Leeuwin Current with tropical oeanic communities"),
                    now = dplyr::case_when(.data$StationCode %in% c("DAR", "YON", "NSI", "PHB", "MAI", "KAI", "ROT", "VBM") ~ "and is ongoing",
                                           .data$StationCode %in% c("ESP", "NIN") ~ "and concluded in March 2013")) %>%
      dplyr::select(-c("ProjectName", "StationCode", "IMCRA"))

  } else {

    # Southern ocean info from https://soe.dcceew.gov.au/antarctica/environment/physical-environment
    # All others from https://www.dcceew.gov.au/environment/marine/marine-bioregional-plans
    CPRinfo <- pr_get_CPRTrips() %>%
      dplyr::summarise(SampleStartDate = as.Date(min(.data$SampleTime_UTC)),
                       Miles = dplyr::n() * 4 * 5,
                       .by = tidyselect::all_of("BioRegion")) %>%
      dplyr::mutate(Features = dplyr::case_when(.data$BioRegion %in% c("South-east") ~ "narrow shelf intensifying currents, eddies and upwellings with low nutrient and primary productivity",
                                                .data$BioRegion %in% c("South-west") ~ "temperate and subtropical habitats influenced by the nutrient deplete Leeuwin Current.",
                                                .data$BioRegion %in% c("Temperate East") ~ "temperate and subtropical habitats influenced by the East Australian Current and its eddies.",
                                                .data$BioRegion %in% c("Coral Sea") ~ "Western Pacific Warm Pool water mass with low surface nutrient levels",
                                                .data$BioRegion %in% c("North-west") ~ "shallow-water tropical marine ecosystems with high species richness, partly driven by the interaction between seafloor features and the currents of the region",
                                                .data$BioRegion %in% c("North") ~ "its high diversity of tropical species but relatively low endemism and provides important bird, marine turtle and dugong breeding, feeding and nursery sites.",
                                                .data$BioRegion %in% c("Southern Ocean Region") ~ "strong biological seasonality and interactions between the atmosphere, ice and ocean that set up patterns of weather and climate that extend across the Southern Hemisphere"))

    }
}
