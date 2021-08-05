
## NRS FUNCTIONS
## Functions for bringing in data sets

#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- get_NRSStation()
#' @importFrom magrittr "%>%"
get_NRSStation <- function(){
  NRSStation <- readr::read_csv(paste0(get_raw_plankton(), "BGC_StationInfo.csv"), na = "") %>%
    dplyr::rename(Station = STATIONNAME, Latitude = LATITUDE, Longitude = LONGITUDE, StationDepth_m = STATIONDEPTH_M) %>%
    dplyr::filter(PROJECTNAME == "NRS")
  return(NRSStation)
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- get_NRSTrips()
#' @importFrom magrittr "%>%"
get_NRSTrips <- function(){
  NRSSamp <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Trip.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, Station = STATION, StationCode = STATIONCODE, Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
                  Biomass_mgm3 = BIOMASS_MGM3, Secchi_m = SECCHI_M, SampleType = SAMPLETYPE) %>%
    dplyr::filter(PROJECTNAME == "NRS") %>%
    dplyr::mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    dplyr::select(TripCode:SampleDateLocal, Year:SampleDateUTC, Biomass_mgm3, Secchi_m, SampleType) %>%
    dplyr::select(-tz)
  return(NRSSamp)
}


#' Import NRS Phytoplankton Data
#'
#' Load NRS station Phytoplankton Data
#' @return A dataframe with NRS Phytoplankton Data
#' @export
#' @examples
#' df <- get_NRSPhytoData()
#' @importFrom magrittr "%>%"
get_NRSPhytoData <- function(){
  NRSPdat <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Phyto_Raw.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, TaxonGroup = TAXON_GROUP, Genus = GENUS, Species = SPECIES,
                  Cells_L = CELL_L, Biovolume_um3L = BIOVOLUME_UM3L)
  return(NRSPdat)
}



#' Import NRS Phytoplankton Changelog
#'
#' Load NRS Phytoplankton Changelog
#' @return A dataframe with NRS Phytoplankton Changelog
#' @export
#' @examples
#' df <- get_NRSPhytoChangeLog()
#' @importFrom magrittr "%>%"
get_NRSPhytoChangeLog <- function(){
  NRSPcl <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Phyto_ChangeLog.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
  return(NRSPcl)
}



#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- get_NRSZooData()
#' @importFrom magrittr "%>%"
get_NRSZooData <- function(){
  NRSZdat <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Zoop_Raw.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01,
                  Genus = GENUS, Species = SPECIES, ZAbund_m3 = ZOOP_ABUNDANCE_M3)
  return(NRSZdat)
}



#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- get_NRSZooCount()
#' @importFrom magrittr "%>%"
get_NRSZooCount <- function(){
  NRSZcount <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Zoop_CountRaw.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01, TripCode = TRIP_CODE,
                  Genus = GENUS, Species = SPECIES, TaxonCount = COUNTS, SampVol_L = SAMPVOL_L)
  return(NRSZcount)
}



#' Load zooplankton NRS Zooplankton changelog
#'
#' @return A dataframe with NRS zooplankton changelog
#' @export
#'
#' @examples
#' df <- get_NRSZooChangeLog()
#' @importFrom magrittr "%>%"
get_NRSZooChangeLog <- function(){
  NRSZcl <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Zoop_ChangeLog.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
  return(NRSZcl)
}


#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- get_NRSPigments()
get_NRSPigments <- function(){
  Pigments <- readr::read_csv(paste0(get_raw_plankton(),"BGC_Pigments.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, PigmentsFlag = PIGMENTS_FLAG, PigmentsComments = PIGMENTS_COMMENTS)
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- get_NRSPico()
#' @importFrom magrittr "%>%"
get_NRSPico <- function(){
  Pico <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Picoplankton.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, Replicate = REPLICATE, SampleDate_Local = SAMPLEDATELOCAL,
                  Prochlorococcus_CellsmL = PROCHLOROCOCCUS_CELLSML, Prochlorococcus_Flag = PROCHLOROCOCCUS_FLAG,
                  Synecochoccus_CellsmL = SYNECOCHOCCUS_CELLSML, Synecochoccus_Flag = SYNECOCHOCCUS_FLAG,
                  Picoeukaryotes_CellsmL = PICOEUKARYOTES_CELLSML, Picoeukaryotes_Flag = PICOEUKARYOTES_FLAG)
  return(Pico)
}




#' Load CTD data
#'
#' @return A dataframe with NRS CTD data
#' @export
#'
#' @examples
#' df <- get_CTD()
#' @importFrom magrittr "%>%"
get_CTD <- function(){
  rawCTD <- readr::read_csv(paste0(get_raw_plankton(), "IMOS_-_Australian_National_Mooring_Network_(ANMN)_-_CTD_Profiles.csv"), na = "", skip = 29,
                            col_types = readr::cols(CHLU = readr::col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
                                                    CHLU_quality_control = readr::col_double(),
                                                    CPHL = readr::col_double(),
                                                    CPHL_quality_control = readr::col_double(),
                                                    cruise_id = readr::col_skip())) %>%
    dplyr::filter(grepl("NRS", site_code)) %>%
    dplyr::mutate(TripCode = ifelse(site_code == 'NRSDAR', paste0(substr(site_code,4,6), format(time_coverage_start, "%Y%m%d_%H:%M")),
                                    paste0(substr(site_code,4,6), format(time_coverage_start, "%Y%m%d"))),
                  StationName = dplyr::case_when(
                    site_code == "NRSDAR" ~ "Darwin",
                    site_code == "NRSYON" ~ "Yongala",
                    site_code == "NRSNSI" ~ "North Stradbroke Island",
                    site_code == "NRSPHB" ~ "Port Hacking",
                    site_code == "NRSMAI" ~ "Maria Island",
                    site_code == "NRSKAI" ~ "Kangaroo Island",
                    site_code == "NRSESP" ~ "Esperance",
                    site_code == "NRSROT" ~ "Rottnest Island",
                    site_code == "NRSNIN" ~ "Ningaloo"),
                  CPHL = ifelse(!is.na(CPHL), CPHL, CHLF)) %>%
    dplyr::rename(CastTime_UTC = time_coverage_start, Latitude = LATITUDE, Longitude = LONGITUDE, Depth_m = DEPTH, Salinity_psu = PSAL,
                  Salinity_flag = PSAL_quality_control, Temperature_degC = TEMP, Temperature_flag = TEMP_quality_control, DissolvedOxygen_umolkg = DOX2,
                  DissolvedOxygen_flag = DOX2_quality_control, Chla_mgm3 = CPHL, Chla_flag = CPHL_quality_control, Turbidity_NTU = TURB,
                  Turbidity_flag = TURB_quality_control, Pressure_dbar = PRES_REL, Conductivity_Sm = CNDC, Conductivity_flag = CNDC_quality_control,
                  WaterDensity_kgm3 = DENS, WaterDensity_flag = DENS_quality_control) %>%
    dplyr::select(file_id, StationName, TripCode, CastTime_UTC, Latitude, Longitude, Depth_m, Salinity_psu, Salinity_flag, Temperature_degC, Temperature_flag,
                  DissolvedOxygen_umolkg, DissolvedOxygen_flag, Chla_mgm3, Chla_flag, Turbidity_NTU, Turbidity_flag, Pressure_dbar, Conductivity_Sm,
                  Conductivity_flag, WaterDensity_kgm3, WaterDensity_flag) %>%
    dplyr::mutate(tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  CastTime_Local = dplyr::case_when(
                    tz == "Australia/Darwin" ~ format(CastTime_UTC, tz = "Australia/Darwin"),
                    tz == "Australia/Brisbane" ~ format(CastTime_UTC, tz = "Australia/Brisbane"),
                    tz == "Australia/Adelaide" ~ format(CastTime_UTC, tz = "Australia/Adelaide"),
                    tz == "Australia/Hobart" ~ format(CastTime_UTC, tz = "Australia/Hobart"),
                    tz == "Australia/Sydney" ~ format(CastTime_UTC, tz = "Australia/Sydney"),
                    tz == "Australia/Perth" ~ format(CastTime_UTC, tz = "Australia/Perth"))) %>%
    dplyr::filter(!file_id %in% c(2117, 2184, 2186, 2187))

  NRSSamp <- get_NRSTrips() %>%
    dplyr::filter(stringr::str_detect(TripCode, "PH4", negate = TRUE))

  Stations <- rawCTD %>%
    dplyr::select(TripCode) %>%
    dplyr::mutate(stations = as.factor(substr(TripCode, 1, 3))) %>%
    dplyr::select(stations) %>%
    dplyr::distinct()

  df <- data.frame(file_id = NA, TripCode = NA)

  for (y in 1:nlevels(Stations$stations)){
    station <- levels(Stations$stations)[[y]]
    rawCTDCast <- rawCTD %>%
      dplyr::select(file_id, CastTime_UTC, TripCode) %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::distinct()

    CastTimes <- rawCTDCast$CastTime_UTC

    Samps <- NRSSamp %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(SampleDateUTC, TripCode) %>%
      dplyr::distinct()

    dateSelect <- function(x){
      which.min(abs(x - CastTimes))
    }

    DateMatch <- sapply(Samps$SampleDateUTC, dateSelect)
    Samps$SampLevel <- DateMatch
    Samps$CastTime_UTC <- Samps$SampleDateUTC

    for (i in 1:nrow(Samps)){
      j <- Samps$SampLevel[[i]]
      Samps$CastTime_UTC[i] <- CastTimes[[j]]
    }

    Samps <- Samps %>%
      dplyr::mutate(DateDiff = abs(CastTime_UTC - SampleDateUTC) / 3600,
                    DateDiff = ifelse(DateDiff > 3 & station != "NSI", NA,
                                      ifelse(DateDiff > 15 & station %in% c("NSI", "KAI"), NA, DateDiff)))

    SampsMatch <- rawCTDCast %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(CastTime_UTC, file_id) %>%
      dplyr::distinct()

    CastMatch <- Samps %>%
      tidyr::drop_na(DateDiff) %>%
      dplyr::inner_join(SampsMatch, by = "CastTime_UTC") %>%
      dplyr::select(file_id, TripCode)

    df <- df %>%
      dplyr::bind_rows(CastMatch) %>%
      tidyr::drop_na()
  }

  rawCTD <- rawCTD %>%
    dplyr::select(-TripCode) %>%
    dplyr::left_join(df, by = "file_id")

}



#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- get_Chemistry()
#' @importFrom magrittr "%>%"
get_Chemistry <- function(){
  chemistry <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Chemistry.csv"), na = c("", NaN),
                               col_types = readr::cols(DIC_UMOLKG = readr::col_double(),
                                                       OXYGEN_UMOLL = readr::col_double(),
                                                       OXYGEN_COMMENTS = readr::col_character())) %>%
    dplyr::rename(TripCode = TRIP_CODE,
                  SampleDepth_m = SAMPLEDEPTH_M, Silicate_umolL = SILICATE_UMOLL, Nitrate_umolL = NITRATE_UMOLL,
                  Phosphate_umolL = PHOSPHATE_UMOLL, Salinity_PSU = SALINITY_PSU,
                  Ammonium_umolL = AMMONIUM_UMOLL,
                  Nitrite_umolL = NITRITE_UMOLL,
                  DIC_umolkg = DIC_UMOLKG,
                  TAlkalinity_umolkg = TALKALINITY_UMOLKG,
                  Oxygen_umolL = OXYGEN_UMOLL) %>%
    dplyr::mutate(SampleDepth_m = as.character(SampleDepth_m),
                  Silicate_umolL = ifelse(SILICATE_FLAG %in% c(3,4,9), NA, Silicate_umolL), # remove all data flagged as bad or probably bad
                  Phosphate_umolL = ifelse(PHOSPHATE_FLAG %in% c(3,4,9), NA, Phosphate_umolL),
                  Ammonium_umolL = ifelse(AMMONIUM_FLAG %in% c(3,4,9), NA, Ammonium_umolL),
                  Nitrate_umolL = ifelse(NITRATE_FLAG %in% c(3,4,9), NA, Nitrate_umolL),
                  Nitrite_umolL = ifelse(NITRITE_FLAG %in% c(3,4,9), NA, Nitrite_umolL),
                  Oxygen_umolL = ifelse(OXYGEN_FLAG %in% c(3,4,9), NA, Oxygen_umolL),
                  DIC_umolkg = ifelse(CARBON_FLAG %in% c(3,4,9), NA, DIC_umolkg),
                  TAlkalinity_umolkg = ifelse(ALKALINITY_FLAG %in% c(3,4,9), NA, TAlkalinity_umolkg),
                  Salinity_PSU = ifelse(SALINITY_FLAG %in% c(3,4,9), NA, Salinity_PSU)) %>%
    dplyr::group_by(TripCode, SampleDepth_m) %>%
    dplyr::summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE), # some replicated samples from error picking up PHB data, needs addressing in database
                     Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
                     Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
                     Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
                     Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
                     Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
                     DIC_umolkg = sum(DIC_umolkg, na.rm = TRUE),
                     TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
                     Salinity_PSU = mean(Salinity_PSU, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    untibble()
  return(chemistry)
}

