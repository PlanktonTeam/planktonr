# Utility functions for retrieving reference/metadata information
#
# This file contains the unified pr_get_info() function for accessing
# plankton taxonomic information and survey policy information.


#' Load plankton taxonomic or survey policy information
#'
#' A unified function to retrieve reference tables containing either taxonomic
#' information for phytoplankton/zooplankton species, or policy-relevant
#' metadata for NRS/CPR surveys.
#'
#' @param Source The type of information to retrieve. Must be one of:
#'   * `"Zooplankton"` or `"Z"` - Zooplankton taxonomic and trait data
#'   * `"Phytoplankton"` or `"P"` - Phytoplankton taxonomic and trait data
#'   * `"NRS"` - National Reference Station policy information
#'   * `"CPR"` - Continuous Plankton Recorder policy information
#'   * `"SOTS"` - Southern Ocean Time Series policy information
#'
#' @details
#' ## Plankton Information (Phytoplankton/Zooplankton)
#' When `Source` is `"Phytoplankton"` or `"Zooplankton"`, returns taxonomic
#' and trait data including:
#' * **Taxonomic classification**: Phylum, class, order, family, genus, species
#' * **Size information**: Length and/or width ranges (micrometres)
#' * **Biovolume**: Estimated cell/organism volume (µm³)
#' * **Carbon content**: Biomass conversion factors (µg C per individual or per cell)
#' * **Functional groups**: Ecological groupings used in [pr_get_FuncGroups()]
#' * **WoRMS identifiers**: AphiaID for linking to World Register of Marine Species
#'
#' ## Survey Policy Information (NRS/CPR/SOTS)
#' When `Source` is `"NRS"`, `"CPR"`, or `"SOTS"`, returns metadata about
#' sampling programs including station/region information, geographic features,
#' and operational status.
#'
#' @return A dataframe with information appropriate to the requested source:
#'   * For plankton types: taxonomic and trait information
#'   * For surveys: policy-relevant metadata
#'
#' @seealso
#' * [pr_get_data()] for abundance data
#' * [pr_get_FuncGroups()] which uses functional group assignments
#'
#' @export
#'
#' @examples
#' # Get zooplankton trait information
#' zoo_info <- pr_get_info(Source = "Zooplankton")
#'
#' # Get phytoplankton trait information
#' phyto_info <- pr_get_info(Source = "Phytoplankton")
#'
#' # Get NRS station policy information
#' nrs_info <- pr_get_info(Source = "NRS")
#'
#' # Get CPR bioregion information
#' cpr_info <- pr_get_info(Source = "CPR")
#'
pr_get_info <- function(Source = "Zooplankton") {

  # Input validation
  assertthat::assert_that(
    is.character(Source) && length(Source) == 1,
    msg = "'Source' must be a single character string. Valid options are 'Zooplankton', 'Phytoplankton', 'NRS', 'CPR', or 'SOTS'."
  )

  # Standardize Source for plankton types
  Source_check <- pr_check_type(Source)

  # Validate Source is one of the allowed values

  assertthat::assert_that(
    Source_check %in% c("Phytoplankton", "Zooplankton") || Source %in% c("NRS", "CPR", "SOTS"),
    msg = "'Source' must be one of 'Zooplankton', 'Phytoplankton', 'NRS', 'CPR', or 'SOTS'."
  )

  # Route to appropriate handler
  if (Source_check %in% c("Phytoplankton", "Zooplankton")) {
    return(.get_plankton_info(Source_check))
  } else {
    return(.get_policy_info(Source))
  }
}


#' Get plankton taxonomic information
#' @param Type "Phytoplankton" or "Zooplankton"
#' @return A dataframe with taxonomic and trait information
#' @noRd
.get_plankton_info <- function(Type) {
  if (Type == "Zooplankton") {
    df <- pr_get_s3("zoopinfo") %>%
      pr_rename()
  } else if (Type == "Phytoplankton") {
    df <- pr_get_s3("phytoinfo") %>%
      pr_rename()
  }
  return(df)
}


#' Get survey policy information
#' @param Survey "NRS", "CPR", or "SOTS"
#' @return A dataframe with policy information
#' @noRd
.get_policy_info <- function(Survey) {

  if (Survey == "NRS") {

    NRSinfo <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE),
                               na = "",
                               show_col_types = FALSE,
                               col_types = readr::cols(
                                 StationStartDate = readr::col_date(format = "%d/%m/%Y"))) %>%
      pr_rename() %>%
      dplyr::mutate(StationCode = ifelse(grepl("SOTS", .data$StationCode), "SOTS", .data$StationCode),
                    StationName = ifelse(grepl("Remote Access Sampler", .data$StationName), "Southern Ocean Time Series", .data$StationName)) %>%
      dplyr::filter(.data$ProjectName == "NRS") %>%
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
                                           .data$StationCode %in% c("ESP", "NIN") ~ "and concluded in March 2013"))

    return(NRSinfo)

  } else if (Survey == "SOTS") {

    SotsInfo <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE),
                                na = "",
                                show_col_types = FALSE,
                                col_types = readr::cols(
                                  StationStartDate = readr::col_date(format = "%d/%m/%Y"))) %>%
      pr_rename() %>%
      dplyr::mutate(StationCode = ifelse(grepl("SOTS", .data$StationCode), "SOTS", .data$StationCode),
                    StationName = ifelse(grepl("Remote Access Sampler", .data$StationName), "Southern Ocean Time Series", .data$StationName)) %>%
      dplyr::filter(.data$ProjectName == "SOTS") %>%
      dplyr::filter(.data$StationCode == "SOTS") %>%
      dplyr::mutate(Region = "Southern Ocean",
                    Features = "deep water moorings in the sub-Antarctic Zone",
                    now = "and is ongoing")

    return(SotsInfo)

  } else if (Survey == "CPR") {

    # Southern ocean info from https://soe.dcceew.gov.au/antarctica/environment/physical-environment
    # All others from https://www.dcceew.gov.au/environment/marine/marine-bioregional-plans
    CPRinfo <- pr_get_trips(Survey = "CPR") %>%
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

    return(CPRinfo)
  }
}
