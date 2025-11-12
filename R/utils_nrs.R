#' Import NRS Data
#'
#' Load NRS station Data
#'
#' @param Type The data of interest: Phytoplankton or Zooplankton
#' @param Variable Variable options are: abundance or biovolume (phytoplankton only)
#' @param Subset Data compilation. Full options are below.
#'
#' @return A dataframe with requested plankton data in wide form
#' @export
#' @importFrom rlang .data
#' @examples
#' df <- pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
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
      dplyr::mutate(StationCode = ifelse(grepl("SOTS", .data$StationCode), "SOTS", .data$StationCode), #TODO - once we get rid of SOTS_RAS we can delete this
                    StationName = ifelse(grepl("Remote Access Sampler", .data$StationName), "Southern Ocean Time Series", .data$StationName)) %>%
      pr_rename()
  }

  # Convert to planktonr class
  # # dat <- planktonr_dat(dat, type = Type, survey = "NRS", variable = Variable, subset = Subset)

  return(dat)

}


#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#'
#' @param Survey NRS is default, could be SOTS
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_Stations('NRS')
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
    dplyr::mutate(StationCode = ifelse(grepl("SOTS", .data$StationCode), "SOTS", .data$StationCode),
                  StationName = ifelse(grepl("Remote Access Sampler", .data$StationName), "Southern Ocean Time Series", .data$StationName)) %>%
    dplyr::filter(.data$ProjectName == Survey)

  return(dat)
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
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


