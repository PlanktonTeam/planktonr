#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_NRSChemistry()
#' @importFrom rlang .data
pr_get_NRSChemistry <- function(){

  var_names <- c("SecchiDepth_m", "Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "DIC_umolkg", "Alkalinity_umolkg", "Salinity") #TODO Check if it is Alk or Total Alk. Our code used to say Total Alk.

  file <- "bgc_chemistry_data"


  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_filter_NRSStations() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local)) %>%
    dplyr::select(.data$Project, .data$SampleTime_Local, .data$Month_Local, .data$SampleDepth_m,
                  .data$StationName, .data$StationCode, tidyselect::all_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'parameters') %>%
    pr_reorder()

  return(dat)

}



#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPigments()
pr_get_NRSPigments <- function(){

  file <- "bgc_pigments_data"
  var_names <- c("TotalChla", "TotalChl", "PPC", "PSC", "PSP", "TCaro", "TAcc", "TPig", "TDP")

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("Pigments_flag") %>%
    dplyr::select(-.data$Pigments_flag) %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    pr_add_StationCode() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TotalChla = sum(.data$CphlideA_mgm3, .data$DvCphlA_mgm3, .data$CphlA_mgm3, na.rm = TRUE),
                  TotalChl = sum(.data$TotalChla, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3, na.rm = TRUE),
                  PPC = sum(.data$Allo_mgm3, .data$Diadchr_mgm3, .data$Diadino_mgm3, .data$Diato_mgm3, .data$Zea_mgm3,  na.rm = TRUE), # CARO, #Photoprotective Carotenoids
                  PSC = sum(.data$Butfuco_mgm3, .data$Hexfuco_mgm3, .data$Perid_mgm3,  na.rm = TRUE), # Photosynthetic Carotenoids
                  PSP = sum(.data$PSC, .data$TotalChl, na.rm = TRUE), # Photosynthetic pigments
                  TCaro = sum(.data$PSC, .data$PSP, na.rm = TRUE), # Total Carotenoids
                  TAcc = sum(.data$TCaro, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3,  na.rm = TRUE), # Total Accessory pigments
                  TPig = sum(.data$TAcc, .data$TotalChla,  na.rm = TRUE), # Total pigments
                  TDP = sum(.data$PSC, .data$Allo_mgm3, .data$Zea_mgm3, .data$DvCphlB_mgm3, .data$CphlB_mgm3,  na.rm = TRUE), # Total Diagnostic pigments
                  StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                  SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
    pr_apply_Time() %>%
    dplyr::filter(.data$TotalChla != 0) %>%
    dplyr::select(.data$Project, .data$SampleTime_Local, .data$Month_Local, .data$SampleDepth_m, .data$StationName, .data$StationCode,
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = 'parameters') %>%
    pr_reorder()

  return(dat)
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()
#' @importFrom rlang .data
pr_get_NRSPico <- function(){

  file <- "bgc_picoplankton_data"

  var_names <- c("Prochlorococcus_cellsmL", "Synecochoccus_cellsmL", "Picoeukaryotes_cellsmL")

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    dplyr::filter(.data$SampleDepth_m != "WC") %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_apply_Time() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
    dplyr::select(.data$Project, .data$SampleTime_Local, .data$Month_Local, .data$Year_Local,
                  .data$SampleDepth_m, .data$StationName, .data$StationCode,
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "parameters") %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    pr_reorder()

  return(dat)
}



#' Load microbial data
#'
#' @return A dataframe with NRS microbial data
#' @export
#'
#' @examples
#' df <- pr_get_NRSMicro()
#' @importFrom rlang .data
pr_get_NRSMicro <- function(){

  var_names <- c("Prochlorococcus_CellsmL", "Synecochoccus_CellsmL", "Picoeukaryotes_CellsmL", "Bacterial_Richness",
                 "Archaeal_Richness", "Eukaryote_Richness", "Bacterial_Niche_Cluster", "Eukaryote_Niche_Cluster", "Archaea_Niche_Cluster",
                 "Bacterial_Chlorophyll_Index", "Bacterial_Nitrogen_Index", "Bacterial_Oxygen_Index", "Bacterial_Phosphate_Index", "Bacterial_Salinity_Index",
                 "Bacterial_Silicate_Index", "Bacterial_Temperature_Index", "Archaeal_Temperature_Index", "Archaeal_Salinity_Index", "Archaeal_Nitrogen_Index",
                 "Archaeal_Phosphate_Index", "Archaeal_Silicate_Index", "Archaeal_Oxygen_Index", "Archaeal_Chlorophyll_Index", "Eukaryote_Temperature_Index",
                 "Eukaryote_Salinity_Index", "Eukaryote_Nitrogen_Index", "Eukaryote_Phosphate_Index", "Eukaryote_Silicate_Index", "Eukaryote_Oxygen_Diversity",
                 "Eukaryote_Chlorophyll_Index")

  dat <- readr::read_csv(system.file("extdata", "datNRSm.csv", package = "planktonr", mustWork = TRUE), na = c("", NA, "NA"), show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = .data$SampleDateLocal,
                  Month_Local = .data$Month,
                  Year_Local = .data$Year,
                  SampleTime_UTC = .data$SampleDateUTC) %>%
    dplyr::mutate(StationName = dplyr::if_else(.data$StationName == "North Stradbroke", "North Stradbroke Island", .data$StationName)) %>%
    pr_add_StationCode() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(.data$TripCode_depth, -3, -1))) %>%
    dplyr::rename(Prochlorococcus_CellsmL = .data$Prochlorococcus_cells_ml,
                  Synecochoccus_CellsmL = .data$Synecochoccus_cells_ml,
                  Picoeukaryotes_CellsmL = .data$Picoeukaryotes_cells_ml) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric)) %>%
    dplyr::select(.data$StationName, .data$SampleDepth_m, .data$StationCode, .data$SampleTime_Local,
                  .data$Year_Local, .data$Month_Local, tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "parameters") %>%
    pr_reorder()

  return(dat)
}




#' Load Total Suspended Solids (TSS) data
#'
#' @return A dataframe with NRS TSS data
#' @export
#'
#' @examples
#' df <- pr_get_NRSTSS()
#' @importFrom rlang .data
pr_get_NRSTSS <- function(){
  file <- "bgc_tss_data"

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("TSSall_flag")

  return(dat)
}


#' Load NRS CTD data
#'
#' @return A dataframe with NRS CTD data
#' @export
#'
#' @examples
#' df <- pr_get_NRSCTD()
#' @importFrom rlang .data
pr_get_NRSCTD <- function(){

  file = "nrs_depth_binned_ctd_data"

  ctd_vars <- c("Project", "file_id", "StationName", "StationCode", "TripCode",
                "SampleTime_Local", "SampleTime_UTC", "Latitude", "Longitude", "SampleDepth_m",
                "Salinity_psu", "Temperature_degC", "DissolvedOxygen_umolkg", "ChlF_mgm3",
                "Turbidity_NTU", "Conductivity_Sm", "WaterDensity_kgm3")

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_add_StationCode() %>%
    dplyr::rename(ChlF_mgm3 = .data$Chla_mgm3) %>%
    #pr_apply_Flags() %>%  #TODO flags are small f, make function work with both
    dplyr::filter(!.data$file_id %in% c(2117, 2184, 2186, 2187)) %>% #TODO Check with Claire
    dplyr::select(tidyselect::all_of(ctd_vars)) %>%
    pr_apply_Time()

  return(dat)
}


#' Get NRS long term nutrient timeseries data
#'
#' @return dataframe for plotting long term nutrient time series info
#' @export
#'
#' @examples
#' df <- pr_get_LTnuts()
pr_get_LTnuts <-  function(){

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "Salinity", "Temperature_degC")

  NutsLT <- readr::read_csv(system.file("extdata", "LongTermMonitoringData.csv", package = "planktonr", mustWork = TRUE),
                            show_col_types = FALSE,
                            na = c("NA", ""),
                            col_types = readr::cols(NITRITE_VALUE = readr::col_number(),
                                                    NITRITE_QC_FLAG = readr::col_number(),
                                                    AMMONIA_VALUE = readr::col_number(),
                                                    AMMONIA_QC_FLAG = readr::col_number())) %>%
    dplyr::mutate(StationCode = gsub(".*[-]([^.]+)[-].*", "\\1", .data$SURVEY_NAME),
                  Project = 'LTM',
                  Month_Local = lubridate::month(.data$START_TIME),
                  Year_Local = lubridate::year(.data$START_TIME)) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = .data$START_TIME,
                  SampleDepth_m = .data$PRESSURE) %>%
    dplyr::select(.data$StationCode, .data$Project, .data$SampleTime_Local, .data$Month_Local, .data$Year_Local,
                  .data$SampleDepth_m, tidyselect::all_of(var_names), tidyselect::contains("_Flag"),
                  -tidyselect::contains("ROSETTE_POSITION")) %>%
    pr_apply_Flags() %>%
    dplyr::select(-dplyr::contains("Flag")) %>%
    pr_add_StationName() %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'parameters') %>%
    dplyr::filter(.data$Values != -999) %>%
    dplyr::relocate(c("Project", "StationName", "StationCode", tidyselect::everything())) %>%
    pr_reorder()

  Nuts <- pr_get_NRSChemistry() %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB"),
                  .data$parameters != 'SecchiDepth_m') %>%
    dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local)) %>%
    dplyr::select(colnames(NutsLT)) # Ensure columns are in the same order

  CTD <- pr_get_NRSCTD() %>%
    dplyr::select(.data$Project, .data$StationCode, .data$StationName, .data$Month_Local, .data$Year_Local,
                  .data$SampleTime_Local, .data$SampleDepth_m, .data$Temperature_degC) %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
    tidyr::pivot_longer(-c(.data$Project:.data$SampleDepth_m), values_to = "Values", names_to = "parameters") %>%
    dplyr::select(colnames(NutsLT))

  dat <- dplyr::bind_rows(NutsLT, Nuts, CTD)

  return(dat)
}


# # Create Biogeochemical data
# #
# # @return A dataframe with BGC data
# # @export
# #
# # @examples
# # df <- pr_get_bgc()
# #
# # @importFrom rlang .data
# pr_get_bgc <- function(){
#
#   # Each trip and depth combination for water quality parameters
#   # the number of rows in this table should equal that in comb, if not look out for duplicates and replicates
#   NRSTrips <- pr_get_NRSTrips() %>%
#     dplyr::select(-.data$SampleType)
#
#   # Hydrochemistry data
#   Chemistry <- pr_get_chemistry()
#
#   # Zooplankton biomass
#   ZBiomass <-  pr_get_NRSTrips() %>%
#     dplyr::select(.data$TripCode, .data$Biomass_mgm3, .data$Secchi_m) %>%
#     dplyr::mutate(SampleDepth_m = "WC")
#
#   # Pigments data
#   Pigments <- pr_get_NRSPigments()
#
#
#   # Flow cytometry picoplankton data
#   Pico <- pr_get_NRSPico() %>%
#     dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
#     dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
#     dplyr::summarise(Prochlorococcus_Cellsml = mean(.data$Prochlorococcus_Cellsml, na.rm = TRUE), # mean of replicates
#                      Synecochoccus_Cellsml = mean(.data$Synecochoccus_Cellsml, na.rm = TRUE),
#                      Picoeukaryotes_Cellsml = mean(.data$Picoeukaryotes_Cellsml, na.rm = TRUE),
#                      .groups = "drop")
#
#   # Total suspended solid data
#   TSS <- pr_get_NRSTSS() %>%
#     dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m),
#                   TripCode = substring(.data$TripCode,4)) %>%
#     dplyr::rename(OrganicFraction_mgL = .data$TSSorganic_mgL,
#                   InorganicFraction_mgL = .data$TSSinorganic_mgL) %>%
#     dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
#     dplyr::summarise(TSS_mgL = mean(.data$TSS_mgL, na.rm = TRUE), # mean of replicates
#                      InorganicFraction_mgL = mean(.data$InorganicFraction_mgL, na.rm = TRUE),
#                      OrganicFraction_mgL = mean(.data$OrganicFraction_mgL, na.rm = TRUE),
#                      .groups = "drop") %>%
#     tidyr::drop_na(.data$SampleDepth_m)
#
#
#   # CTD Cast Data
#   var_names <- c("Density_kgm3", "Temperature_degC", "Conductivity_Sm", "Salinity_psu", "Turbidity_NTU", "CTDChlF_mgm3")
#   CTD <- pr_get_NRSCTD() %>%
#     dplyr::mutate(SampleDepth_m = as.character(round(.data$SampleDepth_m, 0))) %>%
#     dplyr::select(-c(.data$Pressure_dbar)) %>%
#     dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
#     dplyr::summarise(dplyr::across(dplyr::matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
#
#   # combine for all samples taken
#   Samples <- dplyr::bind_rows(Chemistry %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
#                               Pico %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
#                               Pigments %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
#                               TSS %>% dplyr::select(.data$TripCode, .data$SampleDepth_m),
#                               ZBiomass %>% dplyr::select(.data$TripCode, .data$SampleDepth_m)) %>%
#     unique()
#
#   # Combined BGC data for each station at the sample depth
#   BGC <- Samples %>%
#     dplyr::left_join(NRSTrips,  by = c("TripCode")) %>%
#     dplyr::mutate(IMOSsampleCode = paste0('NRS',.data$TripCode, '_', dplyr::if_else(.data$SampleDepth_m == "WC", "WC", stringr::str_pad(.data$SampleDepth_m, 3, side = "left", "0")))) %>%
#     dplyr::left_join(Chemistry, by = c("TripCode", "SampleDepth_m")) %>%
#     dplyr::left_join(Pico, by = c("TripCode", "SampleDepth_m")) %>%
#     dplyr::left_join(Pigments, by = c("TripCode", "SampleDepth_m")) %>%
#     dplyr::left_join(TSS, by = c("TripCode", "SampleDepth_m")) %>%
#     dplyr::left_join(CTD, by = c("TripCode", "SampleDepth_m"))
#
# }
