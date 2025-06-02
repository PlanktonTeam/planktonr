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

  if (Type == "Zooplankton" & Subset == "species"){ # Specific case for zooplankton species

    str <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton")

    datc <- pr_get_Raw("bgc_zooplankton_abundance_copepods_data") %>%
      pr_rename()

    datnc <-pr_get_Raw("bgc_zooplankton_abundance_non_copepods_data") %>%
      pr_rename() %>%
      dplyr::select(-str[!str %in% "TripCode"])

    # Add together and COPEPODS and NON-COPEPODS
    dat <- dplyr::left_join(datc, datnc, by = "TripCode")

  } else {

    file = paste("bgc", tolower(Type), Variable, Subset, "data", sep = "_")
    dat <-pr_get_Raw(file) %>%
      pr_rename()
  }

  # Convert to planktonr class
  dat <- planktonr_dat(dat, type = Type, survey = "NRS", variable = Variable, subset = Subset)

  return(dat)

}


#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#'
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_NRSStation()
#' @importFrom rlang .data
pr_get_NRSStation <- function(){
  dat <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE),
                         na = "",
                         show_col_types = FALSE,
                         col_types = readr::cols(
                           StationStartDate = readr::col_date(format = "%d/%m/%Y"))) %>%
    pr_rename() %>%
    dplyr::filter(.data$ProjectName == "NRS")

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
    planktonr_dat(type = NULL, survey = "NRS", variable = NULL)

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
    dplyr::filter(.data$StationCode %in% c("DAR", "ESP", "KAI", "MAI", "NIN", "NSI", "PHB", "ROT", "YON"))
}


