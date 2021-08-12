#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_Chemistry()
#' @importFrom magrittr "%>%"
pr_get_Chemistry <- function(){
  chemistry <- readr::read_csv(paste0(pr_get_site(), "BGC_Chemistry.csv"), na = c("", NaN),
                               col_types = readr::cols(DIC_UMOLKG = readr::col_double(),
                                                       OXYGEN_UMOLL = readr::col_double(),
                                                       OXYGEN_COMMENTS = readr::col_character())) %>%
    # dplyr::rename(TripCode = TRIP_CODE,
    #               SampleDepth_m = SAMPLEDEPTH_M, Silicate_umolL = SILICATE_UMOLL, Nitrate_umolL = NITRATE_UMOLL,
    #               Phosphate_umolL = PHOSPHATE_UMOLL, Salinity_PSU = SALINITY_PSU,
    #               Ammonium_umolL = AMMONIUM_UMOLL,
    #               Nitrite_umolL = NITRITE_UMOLL,
    #               DIC_umolkg = DIC_UMOLKG,
    #               TAlkalinity_umolkg = TALKALINITY_UMOLKG,
    #               Oxygen_umolL = OXYGEN_UMOLL) %>%
    pr_rename() %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  Silicate_umolL = ifelse(SILICATE_FLAG %in% c(3,4,9), NA, Silicate_umolL), # remove all data flagged as bad or probably bad
                  Phosphate_umolL = ifelse(PHOSPHATE_FLAG %in% c(3,4,9), NA, Phosphate_umolL),
                  Ammonium_umolL = ifelse(AMMONIUM_FLAG %in% c(3,4,9), NA, Ammonium_umolL),
                  Nitrate_umolL = ifelse(NITRATE_FLAG %in% c(3,4,9), NA, Nitrate_umolL),
                  Nitrite_umolL = ifelse(NITRITE_FLAG %in% c(3,4,9), NA, Nitrite_umolL),
                  Oxygen_umolL = ifelse(OXYGEN_FLAG %in% c(3,4,9), NA, Oxygen_umolL),
                  DIC_umolkg = ifelse(CARBON_FLAG %in% c(3,4,9), NA, DIC_umolkg),
                  TAlkalinity_umolkg = ifelse(ALKALINITY_FLAG %in% c(3,4,9), NA, TAlkalinity_umolkg),
                  Salinity_PSU = ifelse(SALINITY_FLAG %in% c(3,4,9), NA, Salinity_PSU)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE), # some replicated samples from error picking up PHB data, needs addressing in database
                     Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
                     Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
                     Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
                     Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
                     Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
                     DIC_umolkg = sum(DIC_umolkg, na.rm = TRUE),
                     TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
                     Salinity_PSU = mean(Salinity_PSU, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA))
}
