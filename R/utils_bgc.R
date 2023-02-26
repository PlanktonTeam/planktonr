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
                 "Oxygen_umolL", "DIC_umolkg", "Alkalinity_umolkg", "Salinity",
                 "NOx_umolL", "DIN_umolL", "Redfield") # TODO Check if it is Alk or Total Alk. Our code used to say Total Alk.

  file <- "bgc_chemistry_data"

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_filter_NRSStations() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local),
                  NOx_umolL = sum(.data$Nitrate_umolL, .data$Nitrite_umolL, na.rm = TRUE),
                  DIN_umolL = sum(.data$NOx_umolL, .data$Ammonium_umolL, na.rm = TRUE),
                  Redfield = mean(.data$NOx_umolL, na.rm = TRUE)/mean(.data$Phosphate_umolL, na.rm = TRUE),
                  Redfield = ifelse(is.infinite(.data$Redfield), NA, .data$Redfield)) %>%
    dplyr::select("Project", "SampleTime_Local", "Month_Local", "SampleDepth_m",
                  "StationName", "StationCode", tidyselect::all_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    pr_reorder()

  return(dat)

}

#' Get pigments data
#' @param Format all or binned
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPigments()
#' df <- pr_get_NRSPigments(Format = "binned")
pr_get_NRSPigments <- function(Format = "all"){

  file <- "bgc_pigments_data"
  if(Format == "binned") {
    var_names <- c("TotalChla", "TotalChl", "PPC", "PSC", "PSP", "TCaro", "TAcc", "TPig", "TDP")
  } else {
    var_names <- c("Allo_mgm3", "AlphaBetaCar_mgm3", "Anth_mgm3", "Asta_mgm3", "BetaBetaCar_mgm3", "BetaEpiCar_mgm3", "Butfuco_mgm3",
                   "Cantha_mgm3", "CphlA_mgm3", "CphlB_mgm3", "CphlC1_mgm3", "CphlC2_mgm3", "CphlC3_mgm3", "CphlC1C2_mgm3",
                   "CphlideA_mgm3", "Diadchr_mgm3", "Diadino_mgm3",  "Fuco_mgm3", "Gyro_mgm3", "Hexfuco_mgm3", "Ketohexfuco_mgm3", "Lut_mgm3",
                   "Lyco_mgm3", "MgDvp_mgm3", "Neo_mgm3", "Perid_mgm3", "PhideA_mgm3", "PhytinA_mgm3", "PhytinB_mgm3", "Pras_mgm3",
                   "PyrophideA_mgm3", "PyrophytinA_mgm3", "Viola_mgm3", "Zea_mgm3", "Pigments_flag" )
  }

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("Pigments_flag") %>%
    dplyr::select(-"Pigments_flag") %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    pr_add_StationCode() %>%
    dplyr::rowwise() %>%
    pr_apply_Time()

  if(Format == "binned") {
    dat <- dat %>%
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
      dplyr::filter(.data$TotalChla != 0)
  }

  dat <- dat %>%
    dplyr::select("Project", "TripCode", "SampleTime_Local", "Month_Local", "SampleDepth_m", "StationName", "StationCode",
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
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

  var_names <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL")
  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    dplyr::filter(.data$SampleDepth_m != "WC") %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_apply_Time() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
    dplyr::select("Project", "SampleTime_Local", "Month_Local", "Year_Local",
                  "SampleDepth_m", "StationName", "StationCode",
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    pr_reorder()

  return(dat)
}

#' Data for environmental contour plots
#'
#' @param Data Use Chemistry, Pico or Micro, the depth stratified samples
#'
#' @return A dataframe to be used with pr_plot_NRSEnvContour
#' @export
#'
#' @examples
#' df <- pr_get_NRSEnvContour('Micro')
pr_get_NRSEnvContour <- function(Data = 'Chemistry') {
  file <- parse(text = paste('planktonr::pr_get_NRS', Data, '()', sep = ''))

  dat <- eval(file) %>%
    dplyr::mutate(name = as.factor(.data$Parameters)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) %>%
    dplyr::filter(.data$Parameters != 'SecchiDepth_m') %>%
    pr_remove_outliers(2) %>%
    droplevels() %>%
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

  var_names <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL",
                 "Bacterial_Nitrogen_Index_KD", "Bacterial_Oxygen_Index_KD", "Bacterial_Phosphate_Index_KD", "Bacterial_Salinity_Index_KD",
                 "Bacterial_Silicate_Index_KD", "Bacterial_Temperature_Index_KD", "Archaeal_Temperature_Index_KD", "Archaeal_Salinity_Index_KD", "Archaeal_Nitrogen_Index_KD",
                 "Archaeal_Phosphate_Index_KD", "Archaeal_Silicate_Index_KD", "Archaeal_Oxygen_Index_KD", "Eukaryote_Temperature_Index_KD",
                 "Eukaryote_Salinity_Index_KD", "Eukaryote_Nitrogen_Index_KD", "Eukaryote_Phosphate_Index_KD", "Eukaryote_Silicate_Index_KD", "Eukaryote_Oxygen_Diversity")

  NRS <- pr_get_NRSTrips() %>%
    dplyr::select("TripCode", "SampleTime_Local", "Year_Local", "Month_Local", "StationName", "StationCode")


  dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv") %>%
    pr_rename() %>%
    dplyr::select("TripCode", "TripCode_depth", tidyselect::any_of(var_names)) %>%
    dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(.data$TripCode_depth, -3, -1))) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric))

  dat <- dat %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
    dplyr::left_join(NRS, by = "TripCode") %>%
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
    dplyr::rename(ChlF_mgm3 = "Chla_mgm3") %>%
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
    dplyr::rename(SampleTime_Local = "START_TIME",
                  SampleDepth_m = "PRESSURE") %>%
    dplyr::select("StationCode", "Project", "SampleTime_Local", "Month_Local", "Year_Local",
                  "SampleDepth_m", tidyselect::all_of(var_names), tidyselect::contains("_Flag"),
                  -tidyselect::contains("ROSETTE_POSITION")) %>%
    pr_apply_Flags() %>%
    dplyr::select(-dplyr::contains("Flag")) %>%
    pr_add_StationName() %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    dplyr::filter(.data$Values != -999) %>%
    dplyr::relocate(c("Project", "StationName", "StationCode", tidyselect::everything())) %>%
    pr_reorder()

  Nuts <- pr_get_NRSChemistry() %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB"),
                  .data$Parameters != 'SecchiDepth_m') %>%
    dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local)) %>%
    dplyr::select(colnames(NutsLT)) # Ensure columns are in the same order

  CTD <- pr_get_NRSCTD() %>%
    dplyr::select("Project", "StationCode", "StationName", "Month_Local", "Year_Local",
                  "SampleTime_Local", "SampleDepth_m", "Temperature_degC") %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
    tidyr::pivot_longer(-c("Project":"SampleDepth_m"), values_to = "Values", names_to = "Parameters") %>%
    dplyr::select(colnames(NutsLT))

  dat <- dplyr::bind_rows(NutsLT, Nuts, CTD)

  return(dat)
}
