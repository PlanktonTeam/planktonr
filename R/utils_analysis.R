#' Fit linear models to plankton time series data
#'
#' Fit linear models to time series data for trend analysis, accounting for both 
#' long-term trends and seasonal cycles. This function is typically used before 
#' extracting coefficients or plotting trends with model predictions.
#'
#' @param dat A dataframe from [pr_get_EOVs()], [pr_get_Indices()], or similar
#'   functions containing time series data. Must contain only one parameter at a time.
#'
#' @details
#' The function fits linear models of the form:
#' 
#' `Values ~ Year + sin(Month) + cos(Month)`
#' 
#' Where:
#' * **Year term** captures long-term linear trends (increasing or decreasing over time)
#' * **Harmonic terms** capture seasonal cycles using Fourier basis functions (k=1, 
#'   allowing for one complete seasonal cycle per year)
#' 
#' The model is fitted separately for each station (NRS/LTM) or bioregion (CPR). 
#' For LTM data, values are first averaged across depths to create a single depth-
#' integrated estimate.
#' 
#' ## Model Interpretation
#' * **Positive Year coefficient**: Variable increasing over time
#' * **Negative Year coefficient**: Variable decreasing over time
#' * **Significant harmonic terms**: Strong seasonal cycle present
#' 
#' The fitted model objects are stored as an attribute of the dataframe and can be 
#' extracted using [pr_get_model()] and analysed using [pr_get_coeffs()].
#' 
#' ## Limitations
#' * Assumes linear trends (non-linear trends may be better captured with GAMs)
#' * Assumes single seasonal cycle (some variables may have sub-annual variation)
#' * Only handles one parameter at a time
#'
#' @returns The input dataframe with model objects stored as a "Model" attribute
#' 
#' @seealso [pr_get_model()] to extract model objects,
#'   [pr_get_coeffs()] to extract tidy coefficients,
#'   [pr_remove_outliers()] to remove outliers before modelling
#' 
#' @export
#'
#' @examples
#' # Fit models to zooplankton biomass
#' dat <- pr_get_EOVs(Survey = "NRS") %>%
#'   dplyr::filter(Parameters == "Biomass_mgm3") %>%
#'   pr_remove_outliers(2) %>%
#'   pr_model_data()
#'
#' # Extract and view model coefficients
#' models <- pr_get_model(dat)
#' coeffs <- pr_get_coeffs(models)
#' print(coeffs)
#'
#' # Fit models to CPR phytoplankton diversity
#' dat_cpr <- pr_get_EOVs(Survey = "CPR") %>%
#'   dplyr::filter(Parameters == "ShannonPhytoDiversity") %>%
#'   pr_remove_outliers(2) %>%
#'   pr_model_data()
pr_model_data <- function(dat){

  # Do one parameter at a time at the moment.
  assertthat::assert_that(
    length(unique(dat$Parameters)) == 1,
    msg = "Only one parameter at a time can be run."
  )

  # Send a message if SOTS data is included that to check sample depths.
  if("StationCode" %in% names(dat)) {
    if(any(grepl("SOTS", dat$StationCode))){
      cat("SOTS samples are taken at varying depths, this model does not account for depth, check your data input")
    }
  }

  Survey <- pr_get_survey(dat)

  if(Survey == "LTM") {
    dat <- dat %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationCode", "StationName", "SampleTime_Local",
                                                  "anomaly", "Year_Local", "Month_Local", "Parameters")))
  }

  # Prepare the data
  lmdat <- dat %>%
    dplyr::rename(SampleDate = "SampleTime_Local") %>%
    dplyr::mutate(Month = .data$Month_Local * 2 * 3.142 / 12) %>%
    droplevels() %>%
    tidyr::drop_na("Values")

  # Set Correct columns/plot titles
  if (Survey == "CPR"){
    site = rlang::sym("BioRegion")
  } else if (Survey != "CPR"){
    site = rlang::sym("StationName")
  }

  site_names <- dat %>%
    dplyr::pull(!!site) %>%
    unique() %>%
    as.character()

  model_list <- site_names %>%
    rlang::set_names() %>%
    purrr::map(~ stats::lm(Values ~ Year_Local + pr_harmonic(Month, k = 1),
                           data = lmdat %>% dplyr::filter(!!site == .x)))

  # Store the model object as a Model attribute with the name of the station
  attr(dat, "Model") <- model_list

  return(dat)

} # End function


#' Extract tidy model coefficients for trend analysis
#'
#' Extract and format model coefficients from fitted linear models, including 
#' significance levels and standard errors. This is useful for creating tables 
#' of trend statistics or identifying significant trends across multiple stations.
#'
#' @param Models A list of model objects extracted using [pr_get_model()] from 
#'   a dataframe that has been processed with [pr_model_data()]
#' @param id Name for the column containing model identifiers (station names or 
#'   bioregion names). Default is `"Station"`.
#'
#' @details
#' The function uses `broom::tidy()` to extract coefficients from each model and 
#' adds significance indicators:
#' * `***` p ≤ 0.001 (highly significant)
#' * `**` p ≤ 0.01 (very significant)
#' * `*` p ≤ 0.05 (significant)
#' * (blank) p > 0.05 (not significant)
#' 
#' For NRS/LTM data, station codes are automatically added if station names are 
#' used as identifiers.
#' 
#' ## Interpreting Coefficients
#' * **Year_Local**: The rate of change per year (slope). Positive = increasing, 
#'   negative = decreasing
#' * **pr_harmonic(Month, k = 1)1**: Sine component of seasonal cycle
#' * **pr_harmonic(Month, k = 1)2**: Cosine component of seasonal cycle
#' * **Intercept**: Expected value at Year = 0 (often not interpretable)
#' 
#' The Year_Local coefficient is typically of most interest for detecting long-term 
#' trends. Significant harmonic terms indicate a strong seasonal cycle.
#'
#' @returns A tibble containing model coefficients with columns:
#'   * Model identifier column (name specified by `id` parameter)
#'   * `term` - Model term name
#'   * `estimate` - Coefficient estimate
#'   * `std.error` - Standard error of estimate
#'   * `statistic` - t-statistic
#'   * `p.value` - P-value
#'   * `signif` - Significance indicator (*, **, ***)
#' 
#' @seealso [pr_model_data()] for fitting models,
#'   [pr_get_model()] for extracting model objects
#' 
#' @export
#'
#' @examples
#' # Fit models and extract coefficients
#' dat <- planktonr::pr_get_EOVs(Survey = "NRS") %>%
#'   dplyr::filter(Parameters == "Biomass_mgm3") %>%
#'   pr_remove_outliers(2) %>%
#'   pr_model_data()
#'
#' models <- pr_get_model(dat)
#' coeffs <- pr_get_coeffs(models)
#' 
#' # View only Year trends
#' coeffs %>% dplyr::filter(term == "Year_Local")
#' 
#' # Identify stations with significant trends
#' coeffs %>% 
#'   dplyr::filter(term == "Year_Local", signif != "")
pr_get_coeffs <- function(Models, id = "Station"){

  # Input validation
  assertthat::assert_that(
    is.list(Models),
    msg = "'Models' must be a list of model objects created by pr_get_model()."
  )

  assertthat::assert_that(
    is.character(id) && length(id) == 1,
    msg = "'id' must be a single character string specifying the name for the model ID column (e.g., 'Station', 'StationName')."
  )

  coefficients <- Models %>%
    purrr::map_dfr(broom::tidy, .id = id) %>%
    dplyr::mutate(signif = dplyr::case_when(p.value <= 0.001 ~ "***",
                                            p.value <= 0.01 ~ "**",
                                            p.value <= 0.05 ~ "*",
                                            .default = ""))

  if (id == "StationName") {
    coefficients <- coefficients %>%
      pr_add_StationCode()
  }

  return(coefficients)

}
