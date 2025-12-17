#' Get sampling trip metadata
#'
#' Load metadata for sampling trips from the National Reference Stations (NRS)
#' or Continuous Plankton Recorder (CPR) survey. This unified function replaces
#' the survey-specific functions `pr_get_NRSTrips()` and `pr_get_CPRTrips()`.
#'
#' @param Survey Survey type to retrieve trips for:
#'   * `"NRS"` - National Reference Station trips (default)
#'   * `"CPR"` - Continuous Plankton Recorder trips
#' @param ... Additional arguments passed to [pr_add_Bioregions()] when 
#'   `Survey = "CPR"`. Currently supports:
#'   * `near_dist_km` - Distance in kilometres to pad bioregion boundaries when
#'     assigning samples to regions (default behaviour uses exact boundaries)
#'
#' @details
#' **NRS Trips:** Returns information about each NRS sampling trip (voyage),
#' including the trip code, station, date/time, and basic metadata. Only NRS
#' project trips are included (SOTS trips are excluded). Samples designated as
#' 'P' (plankton) samples or with no sample type designation are included.
#'
#' **CPR Trips:** The CPR samples continuously as it is towed behind ships of
#' opportunity. Each "trip" represents a segment of the tow, typically covering
#' approximately 3 nautical miles. Samples are automatically assigned to
#' Australian marine bioregions using [pr_add_Bioregions()]. Bioregions include:
#' Temperate East, South-east, South-west, North, and North-west.
#'
#' @return A dataframe with trip information. Common columns include:
#'   * `TripCode` - Unique trip identifier
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `Year_Local`, `Month_Local` - Temporal components
#'   * `Latitude`, `Longitude` - Geographic coordinates
#'
#'   Additional columns vary by survey:
#'   * **NRS:** `StationCode`, `StationName`
#'   * **CPR:** `BioRegion` - Australian marine bioregion assignment
#'
#' @seealso 
#'   [pr_get_info()] for station-level metadata,
#'   [pr_add_Bioregions()] for bioregion assignment details
#'
#' @export
#'
#' @examples
#' # Get NRS trip metadata
#' dat <- pr_get_trips(Survey = "NRS")
#'
#' # Get CPR trip metadata with default bioregion assignment
#' dat <- pr_get_trips(Survey = "CPR")
#'
#' # Get CPR trips with expanded bioregion boundaries (250 km padding)
#' dat <- pr_get_trips(Survey = "CPR", near_dist_km = 250)
#'
#' # Examine sampling frequency by station (NRS)
#' dat <- pr_get_trips(Survey = "NRS")
#' table(dat$StationName, dat$Year_Local)
#'
#' # Examine sampling effort by bioregion and year (CPR)
#' dat <- pr_get_trips(Survey = "CPR")
#' table(dat$BioRegion, dat$Year_Local)
#'
#' @importFrom rlang .data
pr_get_trips <- function(Survey = "NRS", ...) {
  

  # Input validation
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS' or 'CPR'."
  )
  
  assertthat::assert_that(
    Survey %in% c("NRS", "CPR"),
    msg = "'Survey' must be one of 'NRS' or 'CPR'."
  )
  
  if (Survey == "NRS") {
    # Check for unexpected arguments
    dots <- list(...)
    if (length(dots) > 0) {
      warning("Additional arguments are ignored for NRS trips. ",
              "Arguments like 'near_dist_km' are only used for CPR trips.",
              call. = FALSE)
    }
    
    dat <- pr_get_s3("bgc_trip") %>%
      pr_rename() %>%
      dplyr::filter(.data$ProjectName == "NRS" &
                      (stringr::str_detect(.data$SampleType, paste("P", collapse = "|")) |
                         is.na(.data$SampleType))) %>%
      pr_apply_Time() %>%
      dplyr::select(-"ProjectName") %>%
      dplyr::select(-tidyselect::any_of(c("PSampleDepth_m", "ZSampleDepth_m"))) %>%
      planktonr_dat(Type = NULL, Survey = "NRS", Variable = NULL)
    
  } else if (Survey == "CPR") {
    dat <- pr_get_s3("cpr_samp") %>%
      planktonr_dat(Type = NULL, Survey = "CPR", Variable = NULL) %>%
      pr_rename() %>%
      pr_add_Bioregions(...) %>%
      pr_apply_Time()
  }
  
  return(dat)
}
