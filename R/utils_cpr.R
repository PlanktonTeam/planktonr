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

  Type <- pr_check_type(Type)
  
  # Check biovolume is only used with phytoplankton
  assertthat::assert_that(
    !(Variable == "biovolume" && Type == "Zooplankton"),
    msg = "'biovolume' is only available for Phytoplankton data. Please use 'abundance' for Zooplankton."
  )

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
      pr_rename() %>%
      planktonr_dat(Type = Type, Survey = "CPR")
  }

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
  CPRTrips <- pr_get_s3("cpr_samp") %>%
    planktonr_dat(Type = NULL, Survey = "CPR", Variable = NULL) %>%
    pr_rename() %>%
    pr_add_Bioregions() %>%
    pr_apply_Time()
}
