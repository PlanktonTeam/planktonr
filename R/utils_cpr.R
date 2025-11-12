#' Import CPR Data
#'
#' Load Continuous Plankton Recorder (CPR) plankton data from the Integrated Marine 
#' Observing System (IMOS). The CPR is a high-speed plankton sampler that is towed 
#' behind ships of opportunity at approximately 10 metres depth. Samples are collected 
#' continuously along transects, providing broad-scale spatial coverage.
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
#' The CPR samples plankton organisms in the 200-500 micrometre size range. Unlike 
#' NRS data which are point samples, CPR data represent integrated samples over 
#' approximately 3 nautical miles. The data are returned in long format.
#' 
#' CPR sampling occurs both day and night as ships operate continuously. Consider 
#' using [pr_get_DayNight()] to examine diel patterns in the data.
#' 
#' Note: Biovolume data is only available for phytoplankton.
#'
#' @return A dataframe with requested plankton data in long form. Each row represents 
#' a taxon observation in a sample, with columns for metadata (location, date, time) 
#' and abundance or biovolume values.
#' 
#' @seealso [pr_get_NRSData()] for National Reference Station data,
#'   [pr_get_Indices()] for derived ecological indices,
#'   [pr_add_Bioregions()] to assign samples to bioregions
#' 
#' @export
#' @examples
#' # Get raw phytoplankton abundance data
#' df <- pr_get_CPRData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
#' 
#' # Get zooplankton data at species level
#' df <- pr_get_CPRData(Type = "Zooplankton", Variable = "abundance", Subset = "species")
#' 
#' # Get higher taxonomic group data
#' df <- pr_get_CPRData(Type = "Phytoplankton", Variable = "abundance", Subset = "htg")
#' 
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


#' Get CPR sampling trip metadata
#' 
#' Load metadata for all Continuous Plankton Recorder sampling trips, including 
#' dates, locations, and bioregion assignments. Each trip represents a tow 
#' segment sampled by the CPR.
#' 
#' @param ... Additional variables passed to [pr_add_Bioregions()]. Currently supports:
#'   * `near_dist_km` - Distance in kilometres to pad bioregion boundaries when 
#'     assigning samples to regions (default behaviour uses exact boundaries)
#'
#' @details
#' The CPR samples continuously as it is towed behind ships of opportunity. Each 
#' "trip" in this context represents a segment of the tow, typically covering 
#' approximately 3 nautical miles. 
#' 
#' Samples are automatically assigned to Australian marine bioregions using 
#' [pr_add_Bioregions()]. Bioregions include: Temperate East, South-east, 
#' South-west, North, and North-west.
#' 
#' @return A dataframe with CPR trip information including:
#'   * `TripCode` - Unique trip identifier  
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `Year_Local`, `Month_Local` - Temporal components
#'   * `Latitude`, `Longitude` - Geographic coordinates
#'   * `BioRegion` - Australian marine bioregion assignment
#'   * Additional metadata fields
#' 
#' @seealso [pr_get_NRSTrips()] for NRS sampling trips,
#'   [pr_add_Bioregions()] for bioregion assignment details
#' 
#' @export
#'
#' @examples
#' # Get all CPR trips with default bioregion assignment
#' df <- pr_get_CPRTrips()
#' 
#' # Get CPR trips with expanded bioregion boundaries (250 km padding)
#' df <- pr_get_CPRTrips(near_dist_km = 250)
#' 
#' # Examine sampling effort by bioregion and year
#' table(df$BioRegion, df$Year_Local)
#' 
#' @importFrom rlang .data
pr_get_CPRTrips <- function(...){
  CPRTrips <- pr_get_s3("cpr_samp") %>%
    planktonr_dat(Type = NULL, Survey = "CPR", Variable = NULL) %>%
    pr_rename() %>%
    pr_add_Bioregions() %>%
    pr_apply_Time()
}
