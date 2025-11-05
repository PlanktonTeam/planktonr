#' Import NRS Data
#'
#' Load National Reference Station (NRS) plankton data from the Integrated Marine 
#' Observing System (IMOS). The NRS network consists of coastal monitoring stations 
#' around Australia that collect plankton samples, typically on a monthly basis.
#'
#' @param Type The data of interest: `"Phytoplankton"` or `"Zooplankton"`
#' @param Variable Variable options are: 
#'   * `"abundance"` - Cell or individual counts (available for both phytoplankton and zooplankton)
#'   * `"biovolume"` - Cell biovolume in cubic micrometres (phytoplankton only)
#' @param Subset Data compilation level: 
#'   * `"raw"` - Raw taxonomic data as identified by analysts
#'   * `"htg"` - Higher taxonomic groups (e.g., diatoms, dinoflagellates, copepods)
#'   * `"genus"` - Data aggregated to genus level
#'   * `"species"` - Data aggregated to species level
#'   * `"copepods"` - Copepod data only (zooplankton only)
#'
#' @details
#' NRS samples are collected from discrete depth samples using Niskin bottles 
#' (phytoplankton) or vertical net tows (zooplankton). The data are returned in 
#' wide format with taxa as columns and samples as rows.
#' 
#' Note: Biovolume data is only available for phytoplankton, as it is calculated 
#' from cell measurements.
#'
#' @return A dataframe with requested plankton data in wide form. Each row represents 
#' a sample and columns include metadata (station, date, depth) and taxon-specific 
#' abundance or biovolume values.
#' 
#' @seealso [pr_get_CPRData()] for Continuous Plankton Recorder data,
#'   [pr_get_Indices()] for derived ecological indices
#' 
#' @export
#' @importFrom rlang .data
#' @examples
#' # Get raw phytoplankton abundance data
#' df <- pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
#' 
#' # Get zooplankton data at genus level
#' df <- pr_get_NRSData(Type = "Zooplankton", Variable = "abundance", Subset = "genus")
#' 
#' # Get phytoplankton biovolume data
#' df <- pr_get_NRSData(Type = "Phytoplankton", Variable = "biovolume", Subset = "species")
#'
pr_get_NRSData <- function(Type = "Phytoplankton", Variable = "abundance", Subset = "raw"){

  # Input validation
  assertthat::assert_that(
    is.character(Type) && length(Type) == 1,
    msg = "'Type' must be a single character string. Valid options are 'Phytoplankton' or 'Zooplankton'."
  )
  
  assertthat::assert_that(
    is.character(Variable) && length(Variable) == 1,
    msg = "'Variable' must be a single character string. Valid options are 'abundance' or 'biovolume'."
  )
  
  assertthat::assert_that(
    Variable %in% c("abundance", "biovolume"),
    msg = "'Variable' must be one of 'abundance' or 'biovolume'."
  )
  
  assertthat::assert_that(
    is.character(Subset) && length(Subset) == 1,
    msg = "'Subset' must be a single character string. Valid options are 'raw', 'htg', 'genus', 'species', or 'copepods'."
  )
  
  assertthat::assert_that(
    Subset %in% c("raw", "htg", "genus", "species", "copepods"),
    msg = "'Subset' must be one of 'raw', 'htg', 'genus', 'species', or 'copepods'."
  )

  # AVAILABLE NRS DATASETS
  # nrs_phytoplankton_abundance_raw_data
  # nrs_phytoplankton_abundance_htg_data
  # nrs_phytoplankton_abundance_genus_data
  # nrs_phytoplankton_abundance_species_data
  #
  # nrs_phytoplankton_biovolume_raw_data
  # nrs_phytoplankton_biovolume_htg_data
  # nrs_phytoplankton_biovolume_genus_data
  # nrs_phytoplankton_biovolume_species_data
  #
  # nrs_zooplankton_abundance_copepods_data
  # nrs_zooplankton_abundance_non_copepods_data
  # nrs_zooplankton_abundance_genus_data
  # nrs_zooplankton_abundance_htg_data
  # nrs_zooplankton_abundance_raw_data

  Type <- pr_check_type(Type)
  
  # Check biovolume is only used with phytoplankton
  assertthat::assert_that(
    !(Variable == "biovolume" && Type == "Zooplankton"),
    msg = "'biovolume' is only available for Phytoplankton data. Please use 'abundance' for Zooplankton."
  )

  if (Type == "Zooplankton" & Subset == "species"){ # Specific case for zooplankton species

    str <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton")

    datc <- pr_get_Raw("bgc_zooplankton_abundance_copepods_data") %>%
      pr_rename()

    datnc <-pr_get_Raw("bgc_zooplankton_abundance_non_copepods_data") %>%
      pr_rename() %>%
      dplyr::select(-tidyselect::all_of(str[!str %in% "TripCode"]))

    # Add together and COPEPODS and NON-COPEPODS
    dat <- dplyr::left_join(datc, datnc, by = "TripCode")

  } else {

    file = paste("bgc", tolower(Type), Variable, Subset, "data", sep = "_")
    dat <-pr_get_Raw(file) %>%
      pr_rename()
  }

  # Convert to planktonr class
  # # dat <- planktonr_dat(dat, type = Type, survey = "NRS", variable = Variable, subset = Subset)

  return(dat)

}


#' Import NRS Station information
#'
#' Load station metadata including station codes, names, geographic coordinates, 
#' and sampling schedules for National Reference Stations and Southern Ocean 
#' Time Series sites.
#'
#' @param Survey Survey type:
#'   * `"NRS"` - National Reference Stations (default) - includes all active and 
#'     discontinued coastal monitoring stations
#'   * `"SOTS"` - Southern Ocean Time Series mooring site south of Tasmania
#' 
#' @details
#' The National Reference Stations (NRS) network consists of coastal mooring sites 
#' around Australia. Most stations sample monthly, though Darwin samples quarterly 
#' and some stations have discontinued sampling. The returned dataframe includes 
#' station start dates, sampling frequencies, and geographic information.
#' 
#' Station codes include: DAR (Darwin), YON (Yongala), NSI (North Stradbroke Island), 
#' PHB (Port Hacking), MAI (Maria Island), ROT (Rottnest Island), ESP (Esperance), 
#' NIN (Ningaloo), and KAI (Kangaroo Island).
#' 
#' @return A dataframe with station information including:
#'   * `StationCode` - Three-letter station code
#'   * `StationName` - Full station name
#'   * `Latitude`, `Longitude` - Geographic coordinates
#'   * `StationStartDate` - Date sampling commenced
#'   * Additional metadata about sampling schedule and location
#' 
#' @seealso [pr_get_NRSTrips()] for sampling trip metadata
#' 
#' @export
#' @examples
#' # Get all NRS station information
#' df <- pr_get_Stations('NRS')
#' 
#' # Get SOTS station information
#' df <- pr_get_Stations('SOTS')
#' 
#' @importFrom rlang .data
pr_get_Stations <- function(Survey = 'NRS'){
  
  # Input validation
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS' or 'SOTS'."
  )
  
  assertthat::assert_that(
    Survey %in% c("NRS", "SOTS"),
    msg = "'Survey' must be one of 'NRS' or 'SOTS'."
  )
  dat <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE),
                         na = "",
                         show_col_types = FALSE,
                         col_types = readr::cols(
                           StationStartDate = readr::col_date(format = "%d/%m/%Y"))) %>%
    pr_rename() %>%
    dplyr::filter(.data$ProjectName == Survey)

  return(dat)
}


#' Import NRS sampling trip metadata
#'
#' Load metadata for all National Reference Station sampling trips, including 
#' dates, locations, and environmental conditions at the time of sampling.
#' 
#' @details
#' Returns information about each NRS sampling trip (voyage), including the trip 
#' code, station, date/time, and basic metadata. This is useful for understanding 
#' sampling effort over time and for joining with other datasets.
#' 
#' Only NRS project trips are included (SOTS trips are excluded). Samples 
#' designated as 'P' (plankton) samples or with no sample type designation are 
#' included.
#' 
#' @return A dataframe with NRS trip information including:
#'   * `TripCode` - Unique trip identifier
#'   * `StationCode`, `StationName` - Station information
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `Year_Local`, `Month_Local` - Temporal components
#'   * `Latitude`, `Longitude` - Geographic coordinates
#'   * Additional metadata fields
#' 
#' @seealso [pr_get_Stations()] for station-level metadata,
#'   [pr_get_CPRTrips()] for CPR sampling trips
#' 
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
#' 
#' # Examine sampling frequency by station
#' table(df$StationName, df$Year_Local)
#' 
#' @importFrom rlang .data
pr_get_NRSTrips <- function(){

  NRSTrip <- pr_get_s3("bgc_trip") %>%
    pr_rename() %>%
    dplyr::filter(.data$ProjectName == "NRS" &
                    (stringr::str_detect(.data$SampleType, paste("P", collapse = "|")) |
                       is.na(.data$SampleType))) %>%
    pr_apply_Time() %>%
    dplyr::select(-"ProjectName") %>%
    dplyr::select(-tidyselect::any_of(c("PSampleDepth_m", "ZSampleDepth_m"))) %>%
    planktonr_dat(Type = NULL, Survey = "NRS", Variable = NULL)

  return(NRSTrip)

}


# Load zooplankton abundance data
# @return A dataframe with zooplankton abundance data
# @export
#
# @examples
# df <- pr_get_NRSZooData()
# @importFrom rlang .data
# pr_get_NRSZooData <- function(){
#   dat <- readr::read_csv(system.file("extdata", "BGC_Zoop_Raw.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
#     pr_rename()
# }



#' Filter dataframe on `StationCode` to return only NRS stations
#'
#'
#' @param df Dataframe to filter
#'
#' @return Dataframe with only NRS Stations
#' @export
#'
#' @examples
#' \dontrun{df <- pr_filter_NRSStations(df)}
pr_filter_NRSStations <- function(df){

  df <- df %>%
    dplyr::filter(.data$StationCode %in% c("DAR", "ESP", "KAI", "MAI", "NIN", "NSI", "PHB", "ROT", "VBM", "YON"))
}


