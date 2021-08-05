## IMOS BGC Combined Water Quality Parameters
## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)

## Created: Aug 2020
## Updated:
## 24 Sept 2020 (Written to Git)
## 6th October 2020
#
################################
## Bring in data for combined water quality
################################

#' Create Biogeochemical data
#'
#' @return
#' @export
#'
#' @examples
#' df <- create_bgc()
#'
#' @importFrom magrittr "%>%"
create_bgc <- function(){

  # Each trip and depth combination for water quality parameters
  # the number of rows in this table should equal that in comb, if not look out for duplicates and replicates
  NRSTrips <- getNRSTrips() %>%
    dplyr::select(-SampleType)

  # you will get a warning about the fast method, this actually works better than the accurate method for this data set.

  # Hydrochemistry data
  Chemistry <- getChemistry()

  # Zooplankton biomass
  ZBiomass <-  getNRSTrips() %>%
    dplyr::select(TripCode, Biomass_mgm3, Secchi_m) %>%
    dplyr::mutate(SampleDepth_m = 'WC')

  # Pigments data
  Pigments <- readr::read_csv(paste0(get_raw_plankton(),"BGC_Pigments.csv"), na = "(null)") %>%
    dplyr::rename(TripCode = TRIP_CODE,
                  SampleDepth_m = SAMPLEDEPTH_M) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m)) %>%
    dplyr::filter(PIGMENTS_FLAG %in% c(0,1,2,5,8)) %>% # keep data flagged as good
    dplyr::select(-c(PIGMENTS_FLAG, PIGMENTS_COMMENTS)) %>%
    untibble()

  # Flow cytometry picoplankton data
  Pico <- readr::read_csv(paste0(get_raw_plankton(),"BGC_Picoplankton.csv"), na = "(null)") %>%
    dplyr::rename(TripCode = TRIP_CODE,
                  SampleDepth_m = SAMPLEDEPTH_M, Prochlorococcus_cellsml = PROCHLOROCOCCUS_CELLSML, Synecochoccus_cellsml = SYNECOCHOCCUS_CELLSML,
                  Picoeukaryotes_cellsml = PICOEUKARYOTES_CELLSML) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  Prochlorococcus_cellsml = ifelse(PROCHLOROCOCCUS_FLAG %in% c(3,4,9), NA, Prochlorococcus_cellsml), # remove bad data
                  Synecochoccus_cellsml = ifelse(SYNECOCHOCCUS_FLAG %in% c(3,4,9), NA, Synecochoccus_cellsml),
                  Picoeukaryotes_cellsml = ifelse(PICOEUKARYOTES_FLAG %in% c(3,4,9), NA, Picoeukaryotes_cellsml)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(Prochlorococcus_cellsml = mean(Prochlorococcus_cellsml, na.rm = TRUE), # mean of replicates
                     Synecochoccus_cellsml = mean(Synecochoccus_cellsml, na.rm = TRUE),
                     Picoeukaryotes_cellsml = mean(Picoeukaryotes_cellsml, na.rm = TRUE),
                     .groups = "drop") %>%
    untibble()

  # Total suspended solid data
  TSS <- readr::read_csv(paste0(get_raw_plankton(),"BGC_TSS.csv"), na = "(null)") %>%
    dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, TSS_mgL = TSS_MGL,
                  InorganicFraction_mgL = INORGANICFRACTION_MGL,
                  OrganicFraction_mgL = ORGANICFRACTION_MGL, Secchi_m = SECCHIDEPTH_M) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  TripCode = substring(TripCode,4),
                  TSS_mg_L = ifelse(TSSFLAG %in% c(3,4,9), NA, TSS_mgL), # remove bad data
                  InorganicFraction_mgL = ifelse(TSSFLAG %in% c(3,4,9), NA, InorganicFraction_mgL),
                  OrganicFraction_mgL = ifelse(TSSFLAG %in% c(3,4,9), NA, OrganicFraction_mgL)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), # mean of replicates
                     InorganicFraction_mgL = mean(InorganicFraction_mgL, na.rm = TRUE),
                     OrganicFraction_mgL = mean(OrganicFraction_mgL, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::drop_na(SampleDepth_m) %>%
    untibble()

  # CTD Cast Data
  CTD <- getCTD() %>%
    dplyr::mutate(SampleDepth_m = as.character(round(Depth_m, 0))) %>%
    dplyr::select(-c(Pressure_dbar)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(CTDDensity_kgm3 = mean(WaterDensity_kgm3, na.rm = TRUE),
                     CTDTemperature = mean(Temperature_degC, na.rm = TRUE),
                     CTDConductivity_sm = mean(Conductivity_Sm, na.rm = TRUE),
                     CTDSalinity = mean(Salinity_psu, na.rm = TRUE),
                     CTDChlF_mgm3 = mean(Chla_mgm3, na.rm = TRUE),
                     CTDTurbidity_ntu = mean(Turbidity_NTU, na.rm = TRUE)) %>%
    untibble()

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

  # test table
  # n should be 1, replicates or duplicate samples will have values > 1
  test <- BGC %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(n = dplyr::n(),
                     .groups = "drop")

  return(BGC)
}

# df <- create_bgc()
# Check
# max(test$n)

# save to github
# fwrite(BGC, file = paste0(outD,.Platform$file.sep,"NRS_CombinedWaterQuality.csv"), row.names = FALSE)
