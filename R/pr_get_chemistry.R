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
    pr_apply_flags() %>%
    mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
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
