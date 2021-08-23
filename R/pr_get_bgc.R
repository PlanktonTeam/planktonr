#' Create Biogeochemical data
#'
#' @return A dataframe with BGC data
#' @export
#'
#' @examples
#' df <- pr_get_bgc()
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_bgc <- function(){

  # Each trip and depth combination for water quality parameters
  # the number of rows in this table should equal that in comb, if not look out for duplicates and replicates
  NRSTrips <- pr_get_NRSTrips() %>%
    select(-.data$SampleType)

  # you will get a warning about the fast method, this actually works better than the accurate method for this data set.

  # Hydrochemistry data
  Chemistry <- pr_get_Chemistry()

  # Zooplankton biomass
  ZBiomass <-  pr_get_NRSTrips() %>%
    select(.data$TripCode, .data$Biomass_mgm3, .data$Secchi_m) %>%
    mutate(SampleDepth_m = "WC")

  # Pigments data
  Pigments <- readr::read_csv(paste0(pr_get_site(),"BGC_Pigments.csv"), na = "(null)") %>%
    pr_rename() %>%
    mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    filter(.data$PigmentsFlag %in% c(0,1,2,5,8)) %>% # keep data flagged as good
    select(-c(.data$PigmentsFlag, .data$PigmentsComments))

  # Flow cytometry picoplankton data
  Pico <- pr_get_NRSPico() %>%
    mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    pr_apply_flags() %>%
    group_by(.data$TripCode, .data$SampleDepth_m) %>%
    summarise(Prochlorococcus_Cellsml = mean(.data$Prochlorococcus_Cellsml, na.rm = TRUE), # mean of replicates
              Synecochoccus_Cellsml = mean(.data$Synecochoccus_Cellsml, na.rm = TRUE),
              Picoeukaryotes_Cellsml = mean(.data$Picoeukaryotes_Cellsml, na.rm = TRUE),
              .groups = "drop")

  # Total suspended solid data
  TSS <- readr::read_csv(paste0(pr_get_site(),"BGC_TSS.csv"), na = "(null)") %>%
    pr_rename() %>%
    mutate(SampleDepth_m = as.character(.data$SampleDepth_m),
           TripCode = substring(.data$TripCode,4),
           InorganicFraction_Flag = .data$TSS_Flag, # These flags don't exist but are identical to TSS
           OrganicFraction_Flag = .data$TSS_Flag) %>%
    pr_apply_flags() %>%
    group_by(.data$TripCode, .data$SampleDepth_m) %>%
    summarise(TSS_mgL = mean(.data$TSS_mgL, na.rm = TRUE), # mean of replicates
              InorganicFraction_mgL = mean(.data$InorganicFraction_mgL, na.rm = TRUE),
              OrganicFraction_mgL = mean(.data$OrganicFraction_mgL, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::drop_na(.data$SampleDepth_m)

  # CTD Cast Data
  CTD <- pr_get_CTD() %>%
    mutate(SampleDepth_m = as.character(round(.data$SampleDepth_m, 0))) %>%
    select(-c(.data$Pressure_dbar)) %>%
    group_by(.data$TripCode, .data$SampleDepth_m) %>%
    summarise(CTDDensity_kgm3 = mean(.data$WaterDensity_kgm3, na.rm = TRUE),
              CTDTemperature = mean(.data$Temperature_degC, na.rm = TRUE),
              CTDConductivity_Sm = mean(.data$Conductivity_Sm, na.rm = TRUE),
              CTDSalinity = mean(.data$Salinity_psu, na.rm = TRUE),
              CTDChlF_mgm3 = mean(.data$Chla_mgm3, na.rm = TRUE),
              CTDTurbidity_ntu = mean(.data$Turbidity_NTU, na.rm = TRUE))

  # combine for all samples taken
  Samples <- bind_rows(Chemistry %>% select(.data$TripCode, .data$SampleDepth_m),
                       Pico %>% select(.data$TripCode, .data$SampleDepth_m),
                       Pigments %>% select(.data$TripCode, .data$SampleDepth_m),
                       TSS %>% select(.data$TripCode, .data$SampleDepth_m),
                       ZBiomass %>% select(.data$TripCode, .data$SampleDepth_m)) %>%
    unique()

  # Combined BGC data for each station at the sample depth
  BGC <- Samples %>%
    left_join(NRSTrips,  by = c("TripCode")) %>%
    mutate(IMOSsampleCode = paste0('NRS',.data$TripCode, '_', ifelse(.data$SampleDepth_m == "WC", "WC", stringr::str_pad(.data$SampleDepth_m, 3, side = "left", "0")))) %>%
    left_join(Chemistry, by = c("TripCode", "SampleDepth_m")) %>%
    left_join(Pico, by = c("TripCode", "SampleDepth_m")) %>%
    left_join(Pigments, by = c("TripCode", "SampleDepth_m")) %>%
    left_join(TSS, by = c("TripCode", "SampleDepth_m")) %>%
    left_join(CTD, by = c("TripCode", "SampleDepth_m"))

}
