#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_NRSChemistry()
#' @importFrom rlang .data
pr_get_NRSChemistry <- function(){

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "DIC_umolkg", "TAlkalinity_umolkg", "Salinity_psu")

  file <- "bgc_chemistry_data"

  chemistry <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags() %>%
    dplyr::rename(SampleDepth_m = .data$Depth_m) %>%
    dplyr::mutate(SampleDepth_m = as.character(.data$SampleDepth_m)) %>%
    dplyr::group_by(.data$TripCode, .data$SampleDepth_m) %>%
    dplyr::summarise(dplyr::across(dplyr::matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA))
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

  dat <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags("Pigments_flag") %>%
    dplyr::select_if(!grepl('FLAG', names(.)) & !grepl('COMMENTS', names(.))) %>%
    pr_rename() %>%
    dplyr::rename(SampleDepth_m = .data$Depth_m) %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(CphlC3_mgm3 = 0) %>%
    dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m),
                  TotalChla = sum(.data$CphlideA_mgm3, .data$DvCphlA_mgm3, .data$CphlA_mgm3, na.rm = TRUE),
                  TotalChl = sum(.data$TotalChla, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3, na.rm = TRUE),
                  PPC = sum(.data$Allo_mgm3, .data$Diadchr_mgm3, .data$Diadino_mgm3, .data$Diato_mgm3, .data$Zea_mgm3,  na.rm = TRUE), # CARO, #Photoprotective Carotenoids
                  PSC = sum(.data$Butfuco_mgm3, .data$Hexfuco_mgm3, .data$Perid_mgm3,  na.rm = TRUE), # Photosynthetic Carotenoids
                  PSP = sum(.data$PSC, .data$TotalChl,  na.rm = TRUE), # Photosynthetic pigments
                  TCaro = sum(.data$PSC, .data$PSP,  na.rm = TRUE), # Total Carotenoids
                  TAcc = sum(.data$TCaro, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3,  na.rm = TRUE), # Total Accessory pigments
                  TPig = sum(.data$TAcc, .data$TotalChla,  na.rm = TRUE), # Total pigments
                  TDP = sum(.data$PSC, .data$Allo_mgm3, .data$Zea_mgm3, .data$DvCphlB_mgm3, .data$CphlB_mgm3,  na.rm = TRUE), # Total Diagnostic pigments
                  StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                  Month = lubridate::month(.data$SampleDate_Local)) %>%
    dplyr::filter(.data$TotalChla != 0) %>%
    dplyr::select(.data$Project:.data$SampleDepth_m, .data$TotalChla:.data$Month, -.data$TripCode) %>%
    tidyr::pivot_longer(.data$TotalChla:.data$TDP, values_to = "Values", names_to = 'parameters') %>%
    pr_get_StationName() %>%
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

  dat <- pr_get_raw(file) %>%
    pr_rename() %>%
    pr_apply_flags() %>%
    dplyr::rename(SampleDepth_m = .data$Depth_m,
                  Synecochoccus_Cellsml = .data$Synechoc_cellsmL,
                  Prochlorococcus_Cellsml = .data$Prochlorc_cellsmL,
                  Picoeukaryotes_Cellsml = .data$Picoeukar_cellsmL) %>%
    dplyr::select(.data$TripCode, .data$SampleDate_Local, .data$SampleDepth_m,
                  .data[["Prochlorococcus_Cellsml"]]:.data[["Picoeukaryotes_Cellsml"]]) %>%
    dplyr::mutate(StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                  Year = lubridate::year(.data$SampleDate_Local),
                  Month = lubridate::month(.data$SampleDate_Local)) %>%
    pr_get_StationName() %>%
    tidyr::pivot_longer(.data[["Prochlorococcus_Cellsml"]]:.data[["Picoeukaryotes_Cellsml"]], values_to = "Values", names_to = 'parameters')  %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
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
    dplyr::rename(SampleDepth_m = .data$Depth_m) %>%
    pr_apply_flags("TSSall_flag")
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
#   CTD <- pr_get_CTD() %>%
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
