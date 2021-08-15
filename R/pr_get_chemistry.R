#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_Chemistry()

pr_get_Chemistry <- function(){

  chemistry <- readr::read_csv(paste0(pr_get_site(), "BGC_Chemistry.csv"), na = c("", NaN),
                               col_types = readr::cols(DIC_UMOLKG = readr::col_double(),
                                                       OXYGEN_UMOLL = readr::col_double(),
                                                       OXYGEN_COMMENTS = readr::col_character())) %>%
    pr_rename() %>%
    # rename(TripCode = TRIP_CODE,
    #               SampleDepth_m = SAMPLEDEPTH_M, Silicate_umolL = SILICATE_UMOLL, Nitrate_umolL = NITRATE_UMOLL,
    #               Phosphate_umolL = PHOSPHATE_UMOLL, Salinity_psu = SALINITY_PSU,
    #               Ammonium_umolL = AMMONIUM_UMOLL,
    #               Nitrite_umolL = NITRITE_UMOLL,
    #               DIC_umolkg = DIC_UMOLKG,
    #               TAlkalinity_umolkg = TALKALINITY_UMOLKG,
    #               Oxygen_umolL = OXYGEN_UMOLL) %>%
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

    mutate(SampleDepth_m = as.character(SampleDepth_m),
                  Silicate_umolL = ifelse(SILICATE_FLAG %in% c(3,4,9), NA, Silicate_umolL), # remove all data flagged as bad or probably bad
                  Phosphate_umolL = ifelse(PHOSPHATE_FLAG %in% c(3,4,9), NA, Phosphate_umolL),
                  Ammonium_umolL = ifelse(AMMONIUM_FLAG %in% c(3,4,9), NA, Ammonium_umolL),
                  Nitrate_umolL = ifelse(NITRATE_FLAG %in% c(3,4,9), NA, Nitrate_umolL),
                  Nitrite_umolL = ifelse(NITRITE_FLAG %in% c(3,4,9), NA, Nitrite_umolL),
                  Oxygen_umolL = ifelse(OXYGEN_FLAG %in% c(3,4,9), NA, Oxygen_umolL),
                  DIC_umolkg = ifelse(CARBON_FLAG %in% c(3,4,9), NA, DIC_umolkg),
                  TAlkalinity_umolkg = ifelse(ALKALINITY_FLAG %in% c(3,4,9), NA, TAlkalinity_umolkg),
                  Salinity_psu = ifelse(SALINITY_FLAG %in% c(3,4,9), NA, Salinity_psu)) %>%
    group_by(TripCode, SampleDepth_m) %>%
    summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE), # some replicated samples from error picking up PHB data, needs addressing in database
                     Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
                     Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
                     Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
                     Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
                     Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
                     DIC_umolkg = mean(DIC_umolkg, na.rm = TRUE),
                     TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
                     Salinity_psu = mean(Salinity_psu, na.rm = TRUE),
                     .groups = "drop") %>%
    ungroup() %>%
    mutate_all(~ replace(., is.na(.), NA))
}
