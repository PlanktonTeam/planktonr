#' Create Biogeochemical data
#'
#' @return A dataframe with BGC data
#' @export
#'
#' @examples
#' df <- pr_get_bgc()
#'
#' @importFrom rlang .data
pr_get_bgc <- function(){

  # Each trip and depth combination for water quality parameters
  # the number of rows in this table should equal that in comb, if not look out for duplicates and replicates
  NRSTrips <- pr_get_NRSTrips() %>%
    dplyr::select(-.data$SampleType)

  # Hydrochemistry data
  Chemistry <- pr_get_chemistry()

  # Zooplankton biomass
  ZBiomass <-  pr_get_NRSTrips() %>%
    dplyr::select(.data$TripCode, .data$Biomass_mgm3, .data$Secchi_m) %>%
    dplyr::mutate(SampleDepth_m = "WC")

  # Pigments data
  Pigments <- readr::read_csv(system.file("extdata", "BGC_Pigments.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    dplyr::filter(.data$PigmentsFlag %in% c(0,1,2,5,8)) %>% # keep data flagged as good
    dplyr::select(-c(.data$PigmentsFlag, .data$PigmentsComments))

  # Flow cytometry picoplankton data
  Pico <- pr_get_NRSPico() %>%
    dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    pr_apply_flags() %>%
    dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
    dplyr::summarise(Prochlorococcus_Cellsml = mean(.data$Prochlorococcus_Cellsml, na.rm = TRUE), # mean of replicates
              Synecochoccus_Cellsml = mean(.data$Synecochoccus_Cellsml, na.rm = TRUE),
              Picoeukaryotes_Cellsml = mean(.data$Picoeukaryotes_Cellsml, na.rm = TRUE),
              .groups = "drop")

  # Total suspended solid data
  TSS <- readr::read_csv(system.file("extdata", "BGC_TSS.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m),
           TripCode = substring(.data$TripCode,4),
           InorganicFraction_Flag = .data$TSS_Flag, # These flags don't exist but are identical to TSS
           OrganicFraction_Flag = .data$TSS_Flag) %>%
    pr_apply_flags() %>%
    dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
    dplyr::summarise(TSS_mgL = mean(.data$TSS_mgL, na.rm = TRUE), # mean of replicates
              InorganicFraction_mgL = mean(.data$InorganicFraction_mgL, na.rm = TRUE),
              OrganicFraction_mgL = mean(.data$OrganicFraction_mgL, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::drop_na(.data$SampleDepth_m)


  # CTD Cast Data
  var_names <- c("Density_kgm3", "Temperature_degC", "Conductivity_Sm", "Salinity_psu", "Turbidity_NTU", "CTDChlF_mgm3")
  CTD <- pr_get_CTD() %>%
    dplyr::mutate(SampleDepth_m = as.character(round(.data$SampleDepth_m, 0))) %>%
    dplyr::select(-c(.data$Pressure_dbar)) %>%
    dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
    dplyr::summarise(dplyr::across(dplyr::matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # combine for all samples taken
  Samples <- dplyr::bind_rows(Chemistry %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
                       Pico %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
                       Pigments %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
                       TSS %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
                       ZBiomass %>% dplyr::select(.data$TripCode, .data$SampleDepth_m)) %>%
    unique()

  # Combined BGC data for each station at the sample depth
  BGC <- Samples %>%
    dplyr::left_join(NRSTrips,  by = c("TripCode")) %>%
    dplyr::mutate(IMOSsampleCode = paste0('NRS',.data$TripCode, '_', dplyr::if_else(.data$SampleDepth_m == "WC", "WC", stringr::str_pad(.data$SampleDepth_m, 3, side = "left", "0")))) %>%
    dplyr::left_join(Chemistry, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(Pico, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(Pigments, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(TSS, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(CTD, by = c("TripCode", "SampleDepth_m"))

}
