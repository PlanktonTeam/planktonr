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
#' df <- pr_get_CPRData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
#' @importFrom rlang .data
pr_get_CPRData <- function(Type = "Phytoplankton", Variable = "abundance", Subset = "raw"){

  Type <- pr_check_type(Type)

  if (Type == "Zooplankton" & Subset == "species"){ # Specific case for zooplankton species

    datc <- pr_get_Raw("cpr_zooplankton_abundance_copepods_data") %>%
      pr_rename() %>%
      dplyr::arrange(.data$TripCode, .data$SampleTime_Local)

    datnc <- pr_get_Raw("cpr_zooplankton_abundance_non_copepods_data") %>%
      pr_rename() %>%
      dplyr::arrange(.data$TripCode, .data$SampleTime_Local) %>%
      dplyr::select(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = "Zooplankton")))

    # Add together and COPEPODS and NON-COPEPODS
    dat <- dplyr::bind_cols(datc, datnc)

  } else {
    file = paste("cpr", tolower(Type), Variable, Subset, "data", sep = "_")

    dat <- readr::read_csv(stringr::str_replace(pr_get_Site(), "LAYER_NAME", file),
                           na = "",
                           show_col_types = FALSE,
                           comment = "#") %>%
      pr_rename()
  }

  # Convert to planktonr class
  dat <- planktonr_dat(dat, type = Type, survey = "CPR", variable = Variable, subset = Subset)

}


#' Get CPR trips
#' @param ... ability to choose join for bioregions
#'
#' @return A dataframe with CPR Trips
#' @export
#'
#' @examples
#' df <- pr_get_CPRTrips()
#' df <- pr_get_CPRTrips(near_dist_km = 250)
#' @importFrom rlang .data
pr_get_CPRTrips <- function(...){
  CPRTrips <- pr_get_s3("cpr_samp")  %>%
    pr_rename() %>%
    pr_add_Bioregions() %>%
    pr_apply_Time() %>%
    planktonr_dat(type = NULL, survey = "CPR", variable = NULL)
}


# #' Get CPR samples
# #' @param ... to allow use of join when used within another function
# #'
# #' @return A dataframe with CPR Samples
# #' @export
# #'
# #' @examples
# #' df <- pr_get_CPRSamps(near_dist_km = 250)
# #' df <- pr_get_CPRSamps()
# #'
# #' #' @importFrom rlang .data
# pr_get_CPRSamps <- function(...){
#
#   df <- pr_get_Raw("cpr_derived_indices_data") %>%
#     pr_rename() %>%
#     pr_add_Bioregions(...) %>%
#     dplyr::select(tidyselect::starts_with(c("geometry", "FID", "TripCode", "Latitude", "Longitude", "BioRegion", "DistanceFromBioregion_m",
#                                             "IMCRA", "SampleTime", "SampleDate", "Year", "Month", "Day", "Time", "Region")))
#
# }
