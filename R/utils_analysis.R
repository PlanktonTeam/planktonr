#' Run linear model on station and parameters
#'
#' @param df A dataframe of NR, CPR or LTM
#'
#' @returns A dataframe with the model attribute added
#' @export
#'
#' @examples
#' df <- planktonr::pr_get_EOVs(Survey = "NRS") %>%
#'   dplyr::filter(StationCode != "VBM") %>%
#'   dplyr::filter(Parameters == "PigmentChla_mgm3") %>%
#'   pr_remove_outliers(2) %>%
#'   pr_model_data()
pr_model_data <- function(df){

  # Do one parameter at a time at the moment.
  assertthat::assert_that(
    length(unique(df$Parameters)) == 1,
    msg = "Only one parameter at a time can be run."
    )

  Survey <- pr_get_survey(df)

  if(Survey == "LTM") {
    df <- df %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationCode", "StationName", "SampleTime_Local",
                                                  "anomaly", "Year_Local", "Month_Local", "Parameters")))
  }

  # Prepare the data
  lmdat <- df %>%
    dplyr::rename(SampleDate = "SampleTime_Local") %>%
    dplyr::mutate(Month = .data$Month_Local * 2 * 3.142 / 12) %>%
    droplevels() %>%
    tidyr::drop_na()

  # Set Correct columns/plot titles
  if (Survey == "CPR"){
    site = rlang::sym("BioRegion")
  } else if (Survey != "CPR"){
    site = rlang::sym("StationName")
  }

  site_names <- df %>%
    dplyr::pull(!!site) %>%
    unique() %>%
    as.character()

  model_list <- site_names %>%
    rlang::set_names() %>%
    purrr::map(~ stats::lm(Values ~ Year_Local + pr_harmonic(Month, k = 1),
                    data = lmdat %>% dplyr::filter(!!site == .x)))

  # Store the model object as a Model attribute with the name of the station
  attr(df, "Model") <- model_list

  return(df)

} # End function


#' Extract tidy coefficients from model object
#'
#' @param Models Model object created by `planktonr::pr_model_data`
#'
#' @returns
#'
#' @examples
#' df <- planktonr::pr_get_EOVs(Survey = "NRS") %>%
#'   dplyr::filter(StationCode != "VBM") %>%
#'   dplyr::filter(Parameters == "PigmentChla_mgm3") %>%
#'   pr_remove_outliers(2) %>%
#'   pr_model_data()
#' coeffs <- planktonr:::pr_get_coeffs(pr_get_model(df))
pr_get_coeffs <- function(Models){

  coefficients <- Models %>%
    purrr::map_dfr(broom::tidy, .id = "Station") %>%
    dplyr::mutate(signif = dplyr::case_when(p.value <= 0.001 ~ "***",
                                            p.value <= 0.01 ~ "**",
                                            p.value <= 0.05 ~ "*",
                                            .default = ""))
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

}
