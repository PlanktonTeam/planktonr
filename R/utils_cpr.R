#' Import CPR Data
#'
#' Load CPR Data
#'
#' @param Type The data of interest: Phytoplankton or Zooplankton
#' @param Variable Variable options are: abundance or biovolume (phytoplankton only)
#' @param Subset Data compilation. Full options are below.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_CPRData(Type = "phytoplankton", Variable = "abundance", Subset = "raw")
#' @importFrom rlang .data
pr_get_CPRData <- function(Type = "phytoplankton", Variable = "abundance", Subset = "raw"){

  Type = stringr::str_to_lower(Type)
  if (Type == "p"){Type = "phytoplankton"}
  if (Type == "z"){Type = "zooplankton"}

  if (Type == "zooplankton" & Subset == "species"){ # Specific case for zooplankton species

    datc <- pr_get_raw("cpr_zooplankton_abundance_copepods_data") %>%
      pr_rename() %>%
      dplyr::arrange(.data$TripCode, .data$SampleTime_Local)

    datnc <- pr_get_raw("cpr_zooplankton_abundance_non_copepods_data") %>%
      pr_rename() %>%
      dplyr::arrange(.data$TripCode, .data$SampleTime_Local) %>%
      dplyr::select(-c(.data$TripCode:.data$SampleVolume_m3))

    # Add together and COPEPODS and NON-COPEPODS
    dat <- dplyr::bind_cols(datc, datnc)

  } else {
    file = paste("cpr", Type, Variable, Subset, "data", sep = "_")

    dat <- readr::read_csv(stringr::str_replace(pr_get_site(), "LAYER_NAME", file),
                           na = "",
                           show_col_types = FALSE,
                           comment = "#") %>%
      pr_rename()
  }

  # TODO Removed on Monday 20th June 2022 - Don't think we need this anymore.
  # dat <- dat %>%
    # dplyr::rename(SampleTime_UTC = .data$SampleDate_UTC,
                  # SampleTime_Local = .data$SampleDate_Local) %>%  #TODO Fix this when AODN fixes the headers
    # # pr_add_SampleDate()

}


#' Get CPR trips
#'
#' @return A dataframe with CPR Trips
#' @export
#'
#' @examples
#' df <- pr_get_CPRTrips()
#' @importFrom rlang .data
pr_get_CPRTrips <- function(){
  CPRTrips <- readr::read_csv(system.file("extdata", "CPR_Trip.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get CPR samples
#'
#'
#' @return A dataframe with CPR Samples
#' @export
#'
#' @examples
#' df <- pr_get_CPRSamps()
#' @importFrom rlang .data
pr_get_CPRSamps <- function(){

  df <- pr_get_raw("cpr_derived_indices_data") %>%
    pr_rename() %>%
    # pr_add_SampleDate() %>%
    pr_add_Bioregions() %>%
    dplyr::select(tidyselect::starts_with(c("geometry", "FID", "TripCode", "Latitude", "Longitude", "BioRegion",
                                          "IMCRA", "SampleTime", "SampleDate", "Year", "Month", "Day", "Time", "Region")))

}
