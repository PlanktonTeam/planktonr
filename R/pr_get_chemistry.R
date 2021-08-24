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

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "DIC_umolkg", "TAlkalinity_umolkg", "Salinity_psu")

  chemistry <- readr::read_csv(paste0(pr_get_site(), "BGC_Chemistry.csv"), na = c("", NaN),
                               col_types = readr::cols(DIC_UMOLKG = readr::col_double(),
                                                       OXYGEN_UMOLL = readr::col_double(),
                                                       OXYGEN_COMMENTS = readr::col_character())) %>%
    pr_rename() %>%
    pr_apply_flags() %>%
    mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    group_by(.data$TripCode, .data$SampleDepth_m) %>%
    summarise(across(matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate_all(~ replace(., is.na(.), NA))
}
