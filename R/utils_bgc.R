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

  dat <- pr_get_raw(file) %>%
    dplyr::rename(Phosphate_umolL = .data$Phosphate_umoL) %>%
    pr_rename() %>%
    pr_apply_flags() %>%
    pr_add_StationCode() %>%
    pr_filter_NRSStations() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    dplyr::mutate(Month_Local = lubridate::month(.data$SampleDate_Local)) %>%
    dplyr::select(.data$Project, .data$SampleDate_Local, .data$Month_Local, .data$SampleDepth_m,
                  .data$StationName, .data$StationCode, tidyselect::all_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'parameters') %>%
    pr_reorder()

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

  dat <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags("Pigments_flag") %>%
    dplyr::select(-.data$Pigments_flag) %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    pr_add_StationCode() %>%
    # pr_add_StationName() %>%
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
                  Month_Local = lubridate::month(.data$SampleDate_Local)) %>%
    dplyr::filter(.data$TotalChla != 0) %>%
    dplyr::select(.data$Project, .data$SampleDate_Local, .data$Month_Local, .data$SampleDepth_m, .data$StationName, .data$StationCode,
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = 'parameters') %>%
    pr_reorder()

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

  dat <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags() %>%
    pr_add_StationCode() %>%
    # pr_add_StationName() %>%
    dplyr::mutate(Month_Local = lubridate::month(.data$SampleDate_Local),
                  Year_Local = lubridate::year(.data$SampleDate_Local)) %>%
    dplyr::select(.data$Project, .data$SampleDate_Local, .data$Month_Local, .data$Year_Local,
                  .data$SampleDepth_m, .data$StationName, .data$StationCode,
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "parameters") %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    pr_reorder()
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

  var_names <- c("Prochlorococcus_Cellsml", "Synecochoccus_Cellsml", "Picoeukaryotes_Cellsml", "Bacterial_Richness",
                 "Archaeal_Richness", "Eukaryote_Richness", "Bacterial_Niche_Cluster", "Eukaryote_Niche_Cluster", "Archaea_Niche_Cluster",
                 "Bacterial_Chlorophyll_Index", "Bacterial_Nitrogen_Index", "Bacterial_Oxygen_Index", "Bacterial_Phosphate_Index", "Bacterial_Salinity_Index",
                 "Bacterial_Silicate_Index", "Bacterial_Temperature_Index", "Archaeal_Temperature_Index", "Archaeal_Salinity_Index", "Archaeal_Nitrogen_Index",
                 "Archaeal_Phosphate_Index", "Archaeal_Silicate_Index", "Archaeal_Oxygen_Index", "Archaeal_Chlorophyll_Index", "Eukaryote_Temperature_Index",
                 "Eukaryote_Salinity_Index", "Eukaryote_Nitrogen_Index", "Eukaryote_Phosphate_Index", "Eukaryote_Silicate_Index", "Eukaryote_Oxygen_Diversity",
                 "Eukaryote_Chlorophyll_Index")

  dat <- readr::read_csv(system.file("extdata", "datNRSm.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = .data$SampleDateLocal,
                  Month_Local = .data$Month,
                  Year_Local = .data$Year,
                  SampleTime_UTC = .data$SampleDateUTC) %>%
    pr_add_StationCode() %>%
    # pr_add_SampleDate() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(.data$TripCode_depth, -3, -1))) %>%
    dplyr::rename(Prochlorococcus_Cellsml = .data$Prochlorococcus_cells_ml,
                  Synecochoccus_Cellsml = .data$Synecochoccus_cells_ml,
                  Picoeukaryotes_Cellsml = .data$Picoeukaryotes_cells_ml) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric)) %>%
    dplyr::select(.data$StationName, .data$SampleDepth_m, .data$StationCode, .data$SampleTime_Local,
                  .data$Year_Local, .data$Month_Local, tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "parameters") %>%
    pr_reorder()

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

  dat <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags("TSSall_flag")
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

  file = "anmn_ctd_profiles_data"

  rawCTD <- pr_get_raw(file) %>%
    pr_rename() %>%
    dplyr::rename(SampleDepth_m = .data$Depth_m,
                  SampleTime_UTC = .data$SampleDate_UTC) %>% # TODO Hopefully this will be fixed by AODN
    dplyr::mutate(StationCode = stringr::str_remove(.data$site_code, "NRS"), # Have to do this manually due to `NRS` in code
                  Project = "NRS") %>%
    pr_add_StationName() %>%
    pr_filter_NRSStations() %>%
    dplyr::mutate(TripCode = dplyr::if_else(.data$StationCode == "DAR", paste0(substr(.data$StationCode,1,3), format(.data$SampleTime_UTC, "%Y%m%d_%H:%M")),
                                            paste0(substr(.data$StationCode,1,3), format(.data$SampleTime_UTC, "%Y%m%d"))),
                  ChlF_mgm3 = dplyr::if_else(!is.na(.data$Chla_mgm3), .data$Chla_mgm3, .data$ChlF_mgm3)) %>%
    dplyr::select(.data$Project, .data$file_id, .data$StationName, .data$StationCode, .data$TripCode,
                  .data$SampleTime_UTC, .data$Latitude, .data$Longitude, .data$SampleDepth_m,
                  .data$Salinity_psu, .data$Salinity_flag, .data$Temperature_degC, .data$Temperature_flag,
                  .data$DissolvedOxygen_umolkg, .data$DissolvedOxygen_flag, .data$ChlF_mgm3, .data$ChlF_flag,
                  .data$Turbidity_NTU, .data$Turbidity_flag, .data$Pressure_dbar, .data$Conductivity_Sm,
                  .data$Conductivity_flag, .data$WaterDensity_kgm3, .data$WaterDensity_flag) %>%
    pr_apply_flags() %>%
    dplyr::mutate(tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE),
                  SampleTime_Local = dplyr::case_when(
                    .data$tz == "Australia/Darwin" ~ format(.data$SampleTime_UTC, tz = "Australia/Darwin"),
                    .data$tz == "Australia/Brisbane" ~ format(.data$SampleTime_UTC, tz = "Australia/Brisbane"),
                    .data$tz == "Australia/Adelaide" ~ format(.data$SampleTime_UTC, tz = "Australia/Adelaide"),
                    .data$tz == "Australia/Hobart" ~ format(.data$SampleTime_UTC, tz = "Australia/Hobart"),
                    .data$tz == "Australia/Sydney" ~ format(.data$SampleTime_UTC, tz = "Australia/Sydney"),
                    .data$tz == "Australia/Perth" ~ format(.data$SampleTime_UTC, tz = "Australia/Perth"))) %>%
    dplyr::filter(!.data$file_id %in% c(2117, 2184, 2186, 2187))

  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::filter(stringr::str_detect(.data$TripCode, "PH4", negate = TRUE))

  Stations <- rawCTD %>%
    dplyr::mutate(stations = as.factor(.data$StationCode)) %>%
    dplyr::select(.data$stations) %>%
    dplyr::distinct()

  df <- data.frame(file_id = NA, StationCode = NA)

  for (y in 1:nlevels(Stations$stations)){

    station <- levels(Stations$stations)[[y]]

    rawCTDCast <- rawCTD %>%
      dplyr::select(.data$file_id, .data$SampleTime_UTC, .data$StationCode) %>%
      dplyr::filter(.data$StationCode == station) %>%
      dplyr::distinct()

    CastTimes <- rawCTDCast$SampleTime_UTC

    Samps <- NRSSamp %>%
      dplyr::filter(.data$StationCode == station) %>%
      dplyr::select(.data$SampleTime_UTC, .data$StationCode) %>%
      dplyr::distinct()

    dateSelect <- function(x){
      which.min(abs(x - CastTimes))
    }

    Samps$SampLevel <- sapply(Samps$SampleTime_UTC, dateSelect) #DateMatch
    Samps$SampleTime_UTC <- Samps$SampleTime_UTC

    for (i in 1:nrow(Samps)){
      j <- Samps$SampLevel[[i]]
      Samps$SampleTime_UTC[i] <- CastTimes[[j]]
    }

    Samps <- Samps %>%
      dplyr::mutate(DateDiff = as.numeric(abs(.data$SampleTime_UTC - .data$SampleTime_UTC) / 3600),
                    DateDiff = dplyr::case_when(.data$DateDiff > 3 & station != "NSI" ~ NA_real_,
                                                .data$DateDiff > 15 & station %in% c("NSI", "KAI") ~ NA_real_,
                                                TRUE ~ .data$DateDiff))

    SampsMatch <- rawCTDCast %>%
      dplyr::filter(.data$StationCode == station) %>%
      dplyr::select(.data$SampleTime_UTC, .data$file_id) %>%
      dplyr::distinct()

    CastMatch <- Samps %>%
      tidyr::drop_na(.data$DateDiff) %>%
      dplyr::inner_join(SampsMatch, by = "SampleTime_UTC") %>%
      dplyr::select(.data$file_id, .data$StationCode)

    df <- df %>%
      dplyr::bind_rows(CastMatch) %>%
      tidyr::drop_na()
  }

  rawCTD <- rawCTD %>%
    dplyr::left_join(df, by = c("file_id", "StationCode"))

}



## COMMENTING OUT FOR THE MOMENT. THERE ARE DISCREPANCIES IN THE DATE/TIME NAMES FOR THE GROUPS NEEDED TO MERGE

# Get NRS long term nutrient timeseries data
#
# @return dataframe for plotting long term nutrient time series info
# @export
#
# @examples
# df <- pr_get_LTnuts()
# pr_get_LTnuts <-  function(){
#
#   NutsLT <- readr::read_csv(system.file("extdata", "nuts_longterm_clean2.csv", package = "planktonr", mustWork = TRUE),
#                             show_col_types = FALSE,
#                             na = c("NA", "")) %>%
#     pr_rename() %>%
#     tidyr::pivot_longer(-c(.data$StationCode:.data$SampleDepth_m), values_to = "Values", names_to = "parameters") %>%
#     dplyr::rename(SampleTime_Local = .data$SampleDateLocal) %>%
#     dplyr::mutate(Project = "LTM",
#                   SampleTime_Local = strptime(as.POSIXct(.data$SampleTime_Local), "%Y-%m-%d")) %>%
#     pr_add_StationName() %>%
#     dplyr::relocate(c("Project", "StationName", "StationCode", tidyselect::everything())) %>%
#     pr_reorder()
#
#   Nuts <- pr_get_NRSChemistry() %>%
#     dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
#     dplyr::select(colnames(NutsLT)) # Ensure columns are in the same order
#
#   CTD <- pr_get_NRSCTD() %>%
#     dplyr::select(.data$Project, .data$StationCode, .data$StationName,
#                   .data$SampleTime_Local, .data$SampleDepth_m, .data$Temperature_degC) %>%
#     dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
#     tidyr::pivot_longer(-c(.data$Project:.data$SampleDepth_m), values_to = "Values", names_to = "parameters") %>%
#     # dplyr::mutate(SampleDate_Local = strptime(as.POSIXct(.data$SampleDate_Local), "%Y-%m-%d")) %>% # Commented 21/6/22
#     dplyr::select(colnames(NutsLT))
#
#   LTnuts <- dplyr::bind_rows(NutsLT, Nuts, CTD) %>% #TODO I don't think this is used?
#     pr_apply_time()
#     # dplyr::mutate(Year = lubridate::year(.data$SampleDate_Local),
#     #               Month = lubridate::month(.data$SampleDate_Local))
#
#   rm(NutsLT, Nuts, CTD)
#
#   means <- LTnuts %>%
#     dplyr::select(.data$StationName, .data$parameters, .data$Values) %>%
#     dplyr::group_by(.data$StationName, .data$parameters) %>%
#     dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
#                      sd = stats::sd(.data$Values, na.rm = TRUE),
#                      .groups = "drop")
#
#   Pol <- LTnuts %>%
#     dplyr::left_join(means, by = c("StationName", "parameters")) %>%
#     dplyr::mutate(anomaly = (.data$Values - .data$means)/.data$sd)
#
# }




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
