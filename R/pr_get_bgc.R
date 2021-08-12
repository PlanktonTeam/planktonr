#' Create Biogeochemical data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_bgc()
#'
#' @importFrom magrittr "%>%"
pr_get_bgc <- function(){

  # Each trip and depth combination for water quality parameters
  # the number of rows in this table should equal that in comb, if not look out for duplicates and replicates
  NRSTrips <- pr_get_NRSTrips() %>%
    dplyr::select(-SampleType)

  # you will get a warning about the fast method, this actually works better than the accurate method for this data set.

  # Hydrochemistry data
  Chemistry <- pr_get_Chemistry()

  # Zooplankton biomass
  ZBiomass <-  pr_get_NRSTrips() %>%
    dplyr::select(TripCode, Biomass_mgm3, Secchi_m) %>%
    dplyr::mutate(SampleDepth_m = 'WC')

  # Pigments data
  Pigments <- readr::read_csv(paste0(pr_get_site(),"BGC_Pigments.csv"), na = "(null)") %>%
    pr_rename() %>%
    # dplyr::rename(TripCode = TRIP_CODE,
    #               SampleDepth_m = SAMPLEDEPTH_M) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m)) %>%
    dplyr::filter(PigmentsFlag %in% c(0,1,2,5,8)) %>% # keep data flagged as good
    dplyr::select(-c(PigmentsFlag, PigmentsComments))

  # Flow cytometry picoplankton data
  Pico <- pr_get_NRSPico() %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  Prochlorococcus_Cellsml = ifelse(Prochlorococcus_Flag %in% c(3,4,9), NA, Prochlorococcus_Cellsml), # remove bad data
                  Synecochoccus_Cellsml = ifelse(Synecochoccus_Flag %in% c(3,4,9), NA, Synecochoccus_Cellsml),
                  Picoeukaryotes_Cellsml = ifelse(Picoeukaryotes_Flag %in% c(3,4,9), NA, Picoeukaryotes_Cellsml)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(Prochlorococcus_Cellsml = mean(Prochlorococcus_Cellsml, na.rm = TRUE), # mean of replicates
                     Synecochoccus_Cellsml = mean(Synecochoccus_Cellsml, na.rm = TRUE),
                     Picoeukaryotes_Cellsml = mean(Picoeukaryotes_Cellsml, na.rm = TRUE),
                     .groups = "drop")

  # Total suspended solid data
  TSS <- readr::read_csv(paste0(pr_get_site(),"BGC_TSS.csv"), na = "(null)") %>%
    pr_rename() %>%
    dplyr::rename(TripCode = NRS_TRIP_CODE) %>%
    # dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, TSS_mgL = TSS_MGL,
    #               InorganicFraction_mgL = INORGANICFRACTION_MGL,
    #               OrganicFraction_mgL = ORGANICFRACTION_MGL, Secchi_m = SECCHIDEPTH_M) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  TripCode = substring(TripCode,4),
                  TSS_mg_L = ifelse(TSS_Flag %in% c(3,4,9), NA, TSS_mgL), # remove bad data
                  InorganicFraction_mgL = ifelse(TSS_Flag %in% c(3,4,9), NA, InorganicFraction_mgL),
                  OrganicFraction_mgL = ifelse(TSS_Flag %in% c(3,4,9), NA, OrganicFraction_mgL)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), # mean of replicates
                     InorganicFraction_mgL = mean(InorganicFraction_mgL, na.rm = TRUE),
                     OrganicFraction_mgL = mean(OrganicFraction_mgL, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::drop_na(SampleDepth_m)

  # CTD Cast Data
  CTD <- pr_get_CTD() %>%
    dplyr::mutate(SampleDepth_m = as.character(round(SampleDepth_m, 0))) %>%
    dplyr::select(-c(Pressure_dbar)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(CTDDensity_kgm3 = mean(WaterDensity_kgm3, na.rm = TRUE),
                     CTDTemperature = mean(Temperature_degC, na.rm = TRUE),
                     CTDConductivity_sm = mean(Conductivity_Sm, na.rm = TRUE),
                     CTDSalinity = mean(Salinity_psu, na.rm = TRUE),
                     CTDChlF_mgm3 = mean(Chla_mgm3, na.rm = TRUE),
                     CTDTurbidity_ntu = mean(Turbidity_NTU, na.rm = TRUE))

  # combine for all samples taken
  Samples <- dplyr::bind_rows(Chemistry %>% dplyr::select(TripCode, SampleDepth_m),
                              Pico %>% dplyr::select(TripCode, SampleDepth_m),
                              Pigments %>% dplyr::select(TripCode, SampleDepth_m),
                              TSS %>% dplyr::select(TripCode, SampleDepth_m),
                              ZBiomass %>% dplyr::select(TripCode, SampleDepth_m)) %>%
    unique()

  # Combined BGC data for each station at the sample depth
  BGC <- Samples %>%
    dplyr::left_join(NRSTrips,  by = c("TripCode")) %>%
    dplyr::mutate(IMOSsampleCode = paste0('NRS',TripCode, '_', ifelse(SampleDepth_m == 'WC', 'WC', stringr::str_pad(SampleDepth_m, 3, side = "left", "0")))) %>%
    dplyr::left_join(Chemistry, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(Pico, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(Pigments, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(TSS, by = c("TripCode", "SampleDepth_m")) %>%
    dplyr::left_join(CTD, by = c("TripCode", "SampleDepth_m"))

  return(BGC)
}
