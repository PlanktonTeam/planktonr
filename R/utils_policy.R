
#' Load Essential Ocean Variables (EOVs) for plankton biomass and diversity
#' @param Survey "NRS" or "CPR" or "LTM"
#' @param ... to allow use of join when used within another function
#'
#' @return A dataframe with policy data
#' @export
#'
#' @examples
#' dat <- pr_get_EOVs("NRS")
pr_get_EOVs <- function(Survey = "NRS", ...){

  # Input validation
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS', 'CPR', 'SOTS', or 'LTM'."
  )

  assertthat::assert_that(
    Survey %in% c("NRS", "CPR", "LTM", "SOTS"),
    msg = "'Survey' must be one of 'NRS', 'CPR', 'SOTS' or 'LTM'."
  )

  if(Survey == "CPR") {

    var_names <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                    "ShannonCopepodDiversity", "ShannonPhytoDiversity", "SST", "chl_oc3")

    Polr <- pr_get_Raw("cpr_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_Bioregions(...)

    polSat <- pr_get_SatData("CPR") %>%
      dplyr::select("Sample_ID", "SST", "chl_oc3")

    Polr <-  Polr %>%
      dplyr::left_join(polSat, by = "Sample_ID")

    Pol <- Polr %>%
      dplyr::select(tidyselect::starts_with(c("SampleTime_Local", "Year_Local", "Month_Local", "BioRegion", "DistanceFromBioregion_m")),
                    tidyselect::all_of(var_names)) %>%
      dplyr::filter(!.data$BioRegion %in% c("North", "North-west")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select("BioRegion", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("BioRegion", "Parameters")))

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("BioRegion", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = "CPR") %>%
      pr_reorder()

  } else if (Survey == "NRS"){

    var_names <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL",
                   "CTDTemperature_degC", "ShannonCopepodDiversity",
                   "ShannonPhytoDiversity", "Salinity", "PigmentChla_mgm3",
                   "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL", "Silicate_umolL",
                   "Phosphate_umolL", "Oxygen_umolL")

    Polr <- pr_get_Raw("nrs_derived_indices_data") %>%
      pr_rename() %>%
      pr_add_StationCode() %>%
      dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local),
                    Year_Local = lubridate::year(.data$SampleTime_Local))

    Pol <- Polr %>%
      dplyr::select("SampleTime_Local", "Year_Local", "Month_Local", "StationName",
                    "StationCode", tidyselect::all_of(var_names)) %>%
      dplyr::filter(!.data$StationName %in% c("Port Hacking 4")) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters")

    means <- Polr %>%
      dplyr::select("StationName", tidyselect::all_of(var_names)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationName", "Parameters")))

    Pol <- Pol %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = "NRS") %>%
      pr_reorder()

  } else if (Survey == "LTM"){

    all_vars <- c("StationName", "StationCode", "SampleTime_Local", "Month_Local", "Year_Local", "Parameters")

    LTnuts <- pr_get_LTnuts() %>%
      dplyr::filter(.data$SampleDepth_m < 11) %>%
      dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(all_vars)) %>%
      dplyr::rename(Values = mean)

    means <- LTnuts %>%
      dplyr::select("StationName", "Parameters", "Values") %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationName", "Parameters")))

    Pol <- LTnuts %>%
      dplyr::left_join(means, by = c("StationName", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - .data$means)/.data$sd,
                    Survey = "LTM") %>%
      pr_reorder()

  } else if (Survey == "SOTS"){
    cat("This may take a few minutes as none of the SOTS data is pre-processed")

    var_names <- c("PhytoBiomassCarbon_pgL","ShannonPhytoDiversity",
                   "Temperature_degC", "Salinity", "ChlF_mgm3",
                   "Nitrate_umolL",  "Silicate_umolL", "ph",
                   "Phosphate_umolL", "DissolvedOxygen_umolL")

    SOTSwater <- planktonr::pr_get_SOTSMoorData(Type = "Physical") %>%
      dplyr::filter(.data$Parameters %in% c("Salinity", "Temperature_degC", "ChlF_mgm3")) # remove duplicate data from below
    NutsSots <- pr_get_SOTSMoorData(Type = "Nutrients") %>%
      dplyr::filter(!.data$Parameters %in% c("Salinity", "Temperature_degC"))  # remove duplicate data from above

    Pol <- pr_get_Indices(Survey = "SOTS", Type = "Phytoplankton") %>%
      dplyr::filter(.data$Parameters %in% var_names) %>%
      dplyr::select(-c("tz", "TripCode", "Latitude", "Longitude")) %>%
      dplyr::mutate(SampleDepth_m = ifelse(.data$SampleDepth_m < 15, 0, 30)) %>%
      dplyr::bind_rows(SOTSwater) %>%
      dplyr::bind_rows(NutsSots)

    means <- Pol %>%
      dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                       sd = stats::sd(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("StationName", "SampleDepth_m", "Parameters")))

    Pol <-  Pol %>%
      dplyr::left_join(means, by = c("StationName", "SampleDepth_m", "Parameters")) %>%
      dplyr::mutate(anomaly = (.data$Values - means)/sd,
                    Survey = "SOTS")
  }

  return(Pol)

}

#' Load policy-relevant plankton indicators and Essential Ocean Variables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_PolicyInfo()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_info()] which provides a unified interface for both plankton
#' taxonomic information and survey policy information.
#'
#' @param Survey "NRS", "CPR", or "SOTS"
#' @param ... Additional arguments (unused, for backward compatibility)
#'
#' @return A dataframe with policy information
#'
#' @seealso [pr_get_info()] for the preferred interface
#'
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_PolicyInfo("CPR")
#'
#' # Use:
#' dat <- pr_get_info(Source = "CPR")
#' }
pr_get_PolicyInfo <- function(Survey = "NRS", ...) {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_PolicyInfo()",
    with = "pr_get_info()"
  )
  pr_get_info(Source = Survey)
}
