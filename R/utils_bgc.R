#' Prepare biogeochemical data for depth-time contour plots
#'
#' @param Data Use Chemistry, Pico or Micro, the depth stratified samples
#'
#' @return A dataframe to be used with pr_plot_NRSEnvContour
#' @export
#'
#' @examples
#' dat <- pr_get_NRSEnvContour(Data = "Micro")
#' @importFrom rlang .data
pr_get_NRSEnvContour <- function(Data = "Chemistry") {

  # Input validation
  assertthat::assert_that(
    is.character(Data) && length(Data) == 1,
    msg = "'Data' must be a single character string. Valid options are 'Chemistry', 'Pico' or 'Micro'."
  )

  assertthat::assert_that(
    Data %in% c("Chemistry", "Pico", "Micro"),
    msg = "'Data' must be one of 'Chemistry', 'Pico', or 'Micro'."
  )

  # Get data using unified pr_get_data function
  # Data parameter values map directly to Type parameter
  dat <- pr_get_data(Survey = "NRS", Type = Data) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) %>%
    dplyr::filter(.data$Parameters != 'SecchiDepth_m') %>%
    pr_remove_outliers(2) %>%
    droplevels() %>%
    pr_reorder()

  return(dat)
}


#' Get NRS long term nutrient timeseries data
#'
#' @return dataframe for plotting long term nutrient time series info
#' @export
#'
#' @examples
#' dat <- pr_get_LTnuts()
pr_get_LTnuts <-  function(){

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "Salinity", "Temperature_degC")

  NutsLT <- readr::read_csv(system.file("extdata", "LongTermMonitoringData.csv", package = "planktonr", mustWork = TRUE),
                            show_col_types = FALSE,
                            na = c("NA", ""),
                            col_types = readr::cols(NITRITE_VALUE = readr::col_number(),
                                                    NITRITE_QC_FLAG = readr::col_number(),
                                                    AMMONIA_VALUE = readr::col_number(),
                                                    AMMONIA_QC_FLAG = readr::col_number())) %>%
    planktonr_dat(Type = "Water", Survey = "LTM") %>%
    dplyr::mutate(StationCode = gsub(".*[-]([^.]+)[-].*", "\\1", .data$SURVEY_NAME),
                  Project = 'LTM',
                  Month_Local = lubridate::month(.data$START_TIME),
                  Year_Local = lubridate::year(.data$START_TIME)) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = "START_TIME",
                  SampleDepth_m = "PRESSURE") %>%
    dplyr::select("StationCode", "Project", "SampleTime_Local", "Month_Local", "Year_Local",
                  "SampleDepth_m", tidyselect::all_of(var_names), tidyselect::contains("_Flag"),
                  -tidyselect::contains("ROSETTE_POSITION")) %>%
    pr_apply_Flags() %>%
    dplyr::select(-dplyr::contains("Flag")) %>%
    pr_add_StationName() %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    dplyr::filter(.data$Values != -999) %>%
    dplyr::relocate(c("Project", "StationName", "StationCode", tidyselect::everything())) %>%
    pr_reorder()

  Nuts <- pr_get_data(Survey = "NRS", Type = "Chemistry") %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB"),
                  .data$Parameters != 'SecchiDepth_m') %>%
    dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local)) %>%
    dplyr::select(tidyselect::all_of(colnames(NutsLT))) # Ensure columns are in the same order

  CTD <- pr_get_data(Survey = "NRS", Type = "CTD") %>%
    dplyr::select("Project", "StationCode", "StationName", "Month_Local", "Year_Local",
                  "SampleTime_Local", "SampleDepth_m", "Temperature_degC") %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
    tidyr::pivot_longer(-c("Project":"SampleDepth_m"), values_to = "Values", names_to = "Parameters") %>%
    dplyr::select(tidyselect::all_of(colnames(NutsLT)))

  dat <- dplyr::bind_rows(NutsLT, Nuts, CTD)

  return(dat)
}
