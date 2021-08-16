#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_Chemistry()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_Chemistry <- function(){
  chemistry <- readr::read_csv(paste0(pr_get_site(), "BGC_Chemistry.csv"), na = c("", NaN),
                               col_types = readr::cols(DIC_UMOLKG = readr::col_double(),
                                                       OXYGEN_UMOLL = readr::col_double(),
                                                       OXYGEN_COMMENTS = readr::col_character())) %>%
    pr_rename() %>%

    # qc_scheme_short_name,flag_value,flag_meaning,flag_description
    # IMOS IODE,0,No QC performed,The level at which all data enter the working archive. They have not yet been quality controlled
    # IMOS IODE,1,Good data,Top quality data in which no malfunctions have been identified and all real features have been verified during the quality control process
    # IMOS IODE,2,Probably good data,Good data in which some features (probably real) are present but these are unconfirmed. Code 2 data are also data in which minor malfunctions may be present but these errors are small and/or can be successfully corrected without seriously affecting the overall quality of the data.
    # IMOS IODE,3,Bad data that are potentially correctable,Suspect data in which unusual,, and probably erroneous features are observed
    # IMOS IODE,4,Bad data,Obviously erroneous values are observed
    # IMOS IODE,5,Value changed,Altered by a QC Centre,, with original values (before the change) preserved in the history record of the profile. eMII discourage the use of this flag. Where data values must be changed (e.g. smoothing of data sets) we strongly prefer that the original data be retained and an additional variable be added to accommodate the interpolated/corrected data values.
    # IMOS IODE,6,Not used,Flag 6 is reserved for future use
    # IMOS IODE,7,Not used,Flag 7 is reserved for future use
    # IMOS IODE,8,Interpolated value,Indicates that data values are interpolated
  # IMOS IODE,9,Missing value,Indicates that the element is missing


  # JDE - 13th AUgust 2021 - Remove 3, 4, 6, 9

  mutate(SampleDepth_m = as.character(.data$SampleDepth_m),
         Silicate_umolL = ifelse(.data$Silicate_Flag %in% c(3,4,9), NA, .data$Silicate_umolL), # remove all data flagged as bad or probably bad
         Phosphate_umolL = ifelse(.data$Phosphate_Flag %in% c(3,4,9), NA, .data$Phosphate_umolL),
         Ammonium_umolL = ifelse(.data$Ammonium_Flag %in% c(3,4,9), NA, .data$Ammonium_umolL),
         Nitrate_umolL = ifelse(.data$Nitrate_Flag %in% c(3,4,9), NA, .data$Nitrate_umolL),
         Nitrite_umolL = ifelse(.data$Nitrite_Flag %in% c(3,4,9), NA, .data$Nitrite_umolL),
         Oxygen_umolL = ifelse(.data$Oxygen_Flag %in% c(3,4,9), NA, .data$Oxygen_umolL),
         DIC_umolkg = ifelse(.data$DIC_Flag %in% c(3,4,9), NA, .data$DIC_umolkg),
         TAlkalinity_umolkg = ifelse(.data$TAlkalinity_Flag %in% c(3,4,9), NA, .data$TAlkalinity_umolkg),
         Salinity_psu = ifelse(.data$Salinity_Flag %in% c(3,4,9), NA, .data$Salinity_psu)) %>%
    group_by(.data$TripCode, .data$SampleDepth_m) %>%
    summarise(Silicate_umolL = mean(.data$Silicate_umolL, na.rm = TRUE), # some replicated samples from error picking up PHB data, needs addressing in database
              Phosphate_umolL = mean(.data$Phosphate_umolL, na.rm = TRUE),
              Ammonium_umolL = mean(.data$Ammonium_umolL, na.rm = TRUE),
              Nitrate_umolL = mean(.data$Nitrate_umolL, na.rm = TRUE),
              Nitrite_umolL = mean(.data$Nitrite_umolL, na.rm = TRUE),
              Oxygen_umolL = mean(.data$Oxygen_umolL, na.rm = TRUE),
              DIC_umolkg = mean(.data$DIC_umolkg, na.rm = TRUE),
              TAlkalinity_umolkg = mean(.data$TAlkalinity_umolkg, na.rm = TRUE),
              Salinity_psu = mean(.data$Salinity_psu, na.rm = TRUE),
              .groups = "drop") %>%
    ungroup() %>%
    mutate_all(~ replace(., is.na(.), NA))
}
