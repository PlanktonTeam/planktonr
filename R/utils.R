
# Testing
# devtools::document()
# pkgload::load_all()
# devtools::check()

## Functions for operating

#' Remove extra tibble info
#'
#' @param tib
#'
#' @return A dataframe with with all the tibble stuff removed
#' @export
#'
#' @examples
#' data(mtcars)
#' tib <- dplyr::as_tibble(mtcars)
#' df <- untibble(tib)
untibble <- function (tib) {
  tib <- data.frame(unclass(tib), check.names = FALSE, stringsAsFactors = FALSE)
  return(tib)
} ## escape the nonsense


#' Get location of raw plankton data
#'
#' Internal function to load the location of the raw plankton data files.
#' @return A string with location of raw plankton data
#' @export
#' @examples
#' file_loc <- get_raw_plankton()
#' @importFrom magrittr "%>%"
get_raw_plankton <- function(){

  raw <- "https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/"
  return(raw)
}

## NRS FUNCTIONS
## Functions for bringing in data sets

#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- getNRSStation()
#' @importFrom magrittr "%>%"
getNRSStation <- function(){
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
#' df <- getNRSTrips()
#' @importFrom magrittr "%>%"
getNRSTrips <- function(){
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
#' df <- getNRSPhytoData()
#' @importFrom magrittr "%>%"
getNRSPhytoData <- function(){
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
#' df <- getNRSPhytoChangeLog()
#' @importFrom magrittr "%>%"
getNRSPhytoChangeLog <- function(){
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
#' df <- getNRSZooData()
#' @importFrom magrittr "%>%"
getNRSZooData <- function(){
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
#' df <- getNRSZooCount()
#' @importFrom magrittr "%>%"
getNRSZooCount <- function(){
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
#' df <- getNRSZooChangeLog()
#' @importFrom magrittr "%>%"
getNRSZooChangeLog <- function(){
  NRSZcl <- readr::read_csv(paste0(get_raw_plankton(), "BGC_Zoop_ChangeLog.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
  return(NRSZcl)
}



#' Load copepod information table with sizes etc.
#'
#' @return A dataframe with NRS zooplankton information
#' @export
#'
#' @examples
#' df <- getZooInfo()
#' @importFrom magrittr "%>%"
getZooInfo <- function(){
  ZInfo <- readr::read_csv(paste0(get_raw_plankton(), "ZoopInfo.csv"), na = "") %>%
    dplyr::rename( "TaxonName" = "TAXON_NAME") %>%
    untibble()
}




#' Load chemistry data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- getChemistry()
#' @importFrom magrittr "%>%"
getChemistry <- function(){
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



#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- getPigments()
getPigments <- function(){
  Pigments <- readr::read_csv(paste0(get_raw_plankton(),"BGC_Pigments.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, PigmentsFlag = PIGMENTS_FLAG, PigmentsComments = PIGMENTS_COMMENTS)
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- getPico()
#' @importFrom magrittr "%>%"
getPico <- function(){
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
#' df <- getCTD()
#' @importFrom magrittr "%>%"
getCTD <- function(){
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

  NRSSamp <- getNRSTrips() %>%
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


############################################################################################################################################
## CPR FUNCTIONS
############################################################################################################################################
# Bring in all CPR samples

#' Get CPR trips
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRTrips()
#' @importFrom magrittr "%>%"
getCPRTrips <- function(){
  CPRTrips <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Trips.csv"), na = "") %>%
    dplyr::rename(TripCode = TRIP_CODE, StartLatitude = STARTLATITUDE, StartLongitude = STARTLONGITUDE,
                  StartSampleDateUTC = STARTSAMPLEDATEUTC, EndSampleDateUTC = ENDSAMPLEDATEUTC,
                  EndLatitude = ENDLATITUDE, EndLongitude = ENDLONGITUDE,
                  StartPort = STARTPORT, EndPort = ENDPORT, Region = REGION, Miles_nm = MILES,
                  VesselName = VESSEL_NAME, Acknowledgements = ACKNOWLEDGEMENTS)
}

#' Get CPR samples
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRSamps()
#' @importFrom magrittr "%>%"
getCPRSamps <- function(){
  CPRSamps <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Samp.csv"), na = "") %>%
    dplyr::rename(Sample = SAMPLE, Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateUTC = SAMPLEDATEUTC,
                  SampleType = SAMPLETYPE, Biomass_mgm3 = BIOMASS_MGM3, TripCode = TRIP_CODE) %>%
    dplyr::filter(!is.na(SampleType)) %>%
    dplyr::mutate(Year = lubridate::year(SampleDateUTC),
                  Month = lubridate::month(SampleDateUTC),
                  Day = lubridate::day(SampleDateUTC),
                  Time_24hr = stringr::str_sub(SampleDateUTC, -8, -1)) %>%
    dplyr::select(c(TripCode, Sample, Latitude:SampleDateUTC, Year:Time_24hr, PCI, Biomass_mgm3, SampleType))
}

#' Get CPR Phytoplankton Abundance data
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoData()
#' @importFrom magrittr "%>%"
getCPRPhytoData <- function(){
  cprPdat <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Phyto_Raw.csv"), na = "") %>%
    dplyr::rename(Sample = SAMPLE, TaxonName = TAXON_NAME, TaxonGroup = TAXON_GROUP, Genus = GENUS, Species = SPECIES, PAbun_m3 = PHYTO_ABUNDANCE_M3,
                  BioVolume_um3m3 = BIOVOL_UM3M3) %>%
    dplyr::select(-c(FOV_COUNT, SAMPVOL_M3))
}

#' Get CPR Phytoplankton Count data
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoCountData()
#' @importFrom magrittr "%>%"
getCPRPhytoCountData <- function(){
  cprPdat <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Phyto_Raw.csv"), na = "") %>%
    dplyr::rename(Sample = SAMPLE, TaxonName = TAXON_NAME, TaxonGroup = TAXON_GROUP, Genus = GENUS, Species = SPECIES, SampVol_m3 = SAMPVOL_M3,
                  FovCount = FOV_COUNT) %>%
    dplyr::select(-c(BIOVOL_UM3M3, PHYTO_ABUNDANCE_M3))
}

#' Get Phyto Change Log
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoChangeLog()
#' @importFrom magrittr "%>%"
getCPRPhytoChangeLog <- function(){
  cprPcl <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Phyto_ChangeLog.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, StartDate = STARTDATE, ParentName = PARENT_NAME)
}

#' Get CPR Zooplankton Count
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooCountData()
#' @importFrom magrittr "%>%"
getCPRZooCountData <- function() {
  CPRZooCount <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Zoop_Raw.csv"), na = "(null)") %>%
    dplyr::rename(TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01, Sample = SAMPLE,
                  Genus= GENUS, Species = SPECIES, TaxonCount = COUNTS, SampVol_m3 = SAMPVOL_M3) %>%
    dplyr::select(-ZOOP_ABUNDANCE_M3)
}

#' Get CPR Zooplankton abundance data
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooData()
#' @importFrom magrittr "%>%"
getCPRZooData <- function(){
  cprZdat <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Zoop_Raw.csv"), na = "") %>%
    dplyr::rename(Sample = SAMPLE, TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01,
                  Genus = GENUS, Species = SPECIES, ZAbund_m3 = ZOOP_ABUNDANCE_M3) %>%
    dplyr::select(-c(COUNTS, SAMPVOL_M3))
}

#' Get CPR zooplankton Change Log
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooChangeLog()
#' @importFrom magrittr "%>%"
getCPRZooChangeLog <- function(){
  cprZcl <- readr::read_csv(paste0(get_raw_plankton(), "CPR_Zoop_ChangeLog.csv"), na = "") %>%
    dplyr::rename(TaxonName = TAXON_NAME, StartDate = STARTDATE, ParentName = PARENT_NAME)
}

################################################################################################################################################
#' Get CPR Phyto raw & pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoRaw()
#' @importFrom magrittr "%>%"
getCPRPhytoRaw <- function(){
  cprRawP <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(getCPRPhytoData(), by = "Sample") %>%
    dplyr::select(c(Sample:TaxonName,PAbun_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    dplyr::mutate(TaxonName = ifelse(is.na(TaxonName), "No taxa found", TaxonName)) %>% # for segments where no phyto was found
    tidyr::pivot_wider(names_from = TaxonName, values_from = PAbun_m3, values_fill = list(PAbun_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-"No taxa found")
}

#' CPR Phyto HTG pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoHTG
#' @importFrom magrittr "%>%"
getCPRPhytoHTG <- function(){
  cprHTGP1 <- getCPRPhytoData() %>% dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGP <-  getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>% dplyr::left_join(cprHTGP1, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  PAbun_m3 = ifelse(is.na(PAbun_m3), 0, PAbun_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = PAbun_m3, values_fill = list(PAbun_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-Sample)
}

#' Get CPR Phyto genus pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoGenus()
#' @importFrom magrittr "%>%"
getCPRPhytoGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- getCPRPhytoData()

  cprPcl <- getCPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species

  cprGenP1 <- cprPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::group_by(Sample, Genus) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!Genus == "")

  cprGenP1 <- cprSamp %>%
    dplyr::left_join(cprGenP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
                  PAbun_m3 = ifelse(is.na(PAbun_m3), 0, PAbun_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenP2 <- cprPdat  %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(Genus != '') %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    dplyr::group_by(Sample, StartDate, Genus) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenP2$Genus)) {
    Gen <- cprGenP2 %>%
      dplyr::select(Genus) %>%
      unique()
    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(cprGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1)  %>%
      droplevels()

    gen <- as.data.frame(cprGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    gen <- cprSamp %>%
      dplyr::left_join(gen, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Genus = replace(Genus, is.na(Genus), Dates$Genus),
                    PAbun_m3 = replace(PAbun_m3, StartDate>SampleDateUTC, -999),
                    PAbun_m3 = replace(PAbun_m3, StartDate<SampleDateUTC & is.na(PAbun_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(PAbun_m3 = sum(PAbun_m3), .groups = "drop") %>%
      as.data.frame()

    cprGenP1 <- dplyr::bind_rows(cprGenP1, gen)
  }

  cprGenP1 <- cprGenP1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PAbun_m3 = max(PAbun_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenP <-  cprGenP1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = PAbun_m3, values_fill = list(PAbun_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Phyto species pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoSpecies()
#' @importFrom magrittr "%>%"
getCPRPhytoSpecies <-  function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- getCPRPhytoData()

  cprPcl <- getCPRPhytoChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species
  cprSpecP1 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName))
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species)) %>%
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3, na.rm = TRUE), .groups = "drop")

  cprSpecP1 <- cprSamp %>%
    dplyr::left_join(cprSpecP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
                  PAbun_m3 = ifelse(is.na(PAbun_m3), 0, PAbun_m3))  %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprSpecP2 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(TaxonName %in% levels(as.factor(cprPcl$TaxonName))
                  & Species != "spp." & !is.na(Species)
                  & !grepl("cf.", Species)) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(TaxonName != '') %>%
    dplyr::mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    dplyr::group_by(Sample, StartDate, TaxonName) %>%
    dplyr::summarise(PAbun_m3 = sum(PAbun_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprSpecP2$TaxonName)) {
    Spe <- cprSpecP2 %>%
      dplyr::select(TaxonName) %>%
      unique()

    Spe <- as.character(Spe$TaxonName[i] %>%
                          droplevels())

    Dates <- as.data.frame(cprSpecP2) %>%
      dplyr::filter(TaxonName == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    spec <- as.data.frame(cprSpecP2) %>%
      dplyr::filter(TaxonName == Spe) %>%
      droplevels()

    spec <- cprSamp %>%
      dplyr::left_join(spec, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
                    PAbun_m3 = replace(PAbun_m3, StartDate>SampleDateUTC, -999),
                    PAbun_m3 = replace(PAbun_m3, StartDate<SampleDateUTC & is.na(PAbun_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
      dplyr::summarise(PAbun_m3 = sum(PAbun_m3), .groups = "drop") %>%
      as.data.frame()
    cprSpecP1 <- dplyr::bind_rows(cprSpecP1, spec)
  }

  cprSpecP1 <- cprSpecP1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PAbun_m3 = max(PAbun_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    as.data.frame()

  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified
  cprSpecP <-  cprSpecP1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = PAbun_m3, values_fill = list(PAbun_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

###############################################################################################################################################
#' Get CPR Phyto raw & pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoRawBV()
#' @importFrom magrittr "%>%"
getCPRPhytoRawBV <- function(){
  cprRawP <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(getCPRPhytoData(), by = "Sample") %>%
    dplyr::select(c(Sample:TaxonName,BioVolume_um3m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    dplyr::mutate(TaxonName = ifelse(is.na(TaxonName), "No taxa found", TaxonName)) %>% # for segments where no phyto was found
    tidyr::pivot_wider(names_from = TaxonName, values_from = BioVolume_um3m3, values_fill = list(BioVolume_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-"No taxa found")
}

#' Get CPR Phyto HTG product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRHTGBV()
#' @importFrom magrittr "%>%"
getCPRHTGBV <- function(){
  cprHTGPB1 <- getCPRPhytoData() %>%
    dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGPB1 <-  getCPRSamps()  %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(cprHTGPB1, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  PBioV_um3m3 = ifelse(is.na(PBioV_um3m3), 0, PBioV_um3m3)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-Sample)
}

#' Get CPR Phyto genus product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoGenusBV()
#' @importFrom magrittr "%>%"
getCPRPhytoGenusBV <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- getCPRPhytoData()

  cprPcl <- getCPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species

  cprGenPB1 <- cprPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName)) & Genus != '') %>%
    dplyr::group_by(Sample, Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  cprGenPB1 <- cprSamp %>%
    dplyr::left_join(cprGenPB1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
                  PBioV_um3m3 = ifelse(is.na(PBioV_um3m3), 0, PBioV_um3m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(PBioV_um3m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenPB2 <- cprPdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(Genus != '') %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>% tidyr::drop_na(Genus) %>%
    dplyr::group_by(Sample, StartDate, Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenPB2$Genus)) {
    Gen <- cprGenPB2 %>% dplyr::select(Genus) %>% unique()
    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(cprGenPB2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1)  %>%
      droplevels()

    gen <- as.data.frame(cprGenPB2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    gen <- cprSamp %>%
      dplyr::left_join(gen, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Genus = replace(Genus, is.na(Genus), Dates$Genus),
                    PBioV_um3m3 = replace(PBioV_um3m3, StartDate>SampleDateUTC, -999),
                    PBioV_um3m3 = replace(PBioV_um3m3, StartDate<SampleDateUTC & is.na(PBioV_um3m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(PBioV_um3m3 = sum(PBioV_um3m3), .groups = "drop") %>%
      as.data.frame()

    cprGenPB1 <- dplyr::bind_rows(cprGenPB1, gen)
  }

  cprGenPB1 <- cprGenPB1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PBioV_um3m3 = max(PBioV_um3m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  cprGenPB <-  cprGenPB1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Phyto species product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRPhytoSpeciesBV()
#' @importFrom magrittr "%>%"
getCPRPhytoSpeciesBV <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- getCPRPhytoData()

  cprPcl <- getCPRPhytoChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species
  cprSpecPB1 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName))
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species)) %>%
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  cprSpecPB1 <- cprSamp %>%
    dplyr::left_join(cprSpecPB1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
                  PBioV_um3m3 = ifelse(is.na(PBioV_um3m3), 0, PBioV_um3m3))  %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(PBioV_um3m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprSpecPB2 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(TaxonName %in% levels(as.factor(cprPcl$TaxonName))
                  & Species != "spp." & !is.na(Species)
                  & !grepl("cf.", Species)) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(TaxonName != '') %>%
    dplyr::mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    dplyr::group_by(Sample, StartDate, TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprSpecPB2$TaxonName)) {
    Spe <- cprSpecPB2 %>% dplyr::select(TaxonName) %>% unique()
    Spe <- as.character(Spe$TaxonName[i] %>% droplevels())

    Dates <- as.data.frame(cprSpecPB2) %>%
      dplyr::filter(TaxonName == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    spec <- as.data.frame(cprSpecPB2) %>%
      dplyr::filter(TaxonName == Spe) %>%
      droplevels()

    spec <- cprSamp %>%
      dplyr::left_join(spec, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
                    PBioV_um3m3 = replace(PBioV_um3m3, StartDate>SampleDateUTC, -999),
                    PBioV_um3m3 = replace(PBioV_um3m3, StartDate<SampleDateUTC & is.na(PBioV_um3m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
      dplyr::summarise(PBioV_um3m3 = sum(PBioV_um3m3), .groups = "drop") %>%
      as.data.frame()
    cprSpecPB1 <- dplyr::bind_rows(cprSpecPB1, spec)
  }

  cprSpecPB1 <- cprSpecPB1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = max(PBioV_um3m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    as.data.frame()

  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified
  cprSpecPB <-  cprSpecPB1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#### CPR Zooplankton #### ################################################################################################################################
#' Get CPR Zoop raw & pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooRaw()
#' @importFrom magrittr "%>%"
getCPRZooRaw <- function(){
  cprRawZ <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(getCPRZooData(), by = "Sample") %>%
    dplyr::select(-c("Copepod", "TaxonGroup", "Genus", "Species", 'SPCODE')) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    dplyr::mutate(TaxonName = ifelse(is.na(TaxonName), "No taxa found", TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-"No taxa found") %>%
    dplyr::select(-Sample)
}

#' CPR Zoop raw product binned by sex and stage raw pivoted product
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooRawSS()
#' @importFrom magrittr "%>%"
getCPRZooRawSS <- function(){
  CPRIdsZ <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(getCPRZooData(), by = "Sample") %>%
    dplyr::mutate(TaxonName = ifelse(is.na(Genus), TaxonName, paste0(Genus, ' ', Species))) %>%
    dplyr::group_by(TripCode, Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName))  %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Zoop HTG pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooHTG()
#' @importFrom magrittr "%>%"
getCPRZooHTG <- function(){
  cprHTGZ <- getCPRZooData() %>%
    dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other"))

  cprHTGZ1 <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(cprHTGZ, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Copepod", TaxonGroup),
                  ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-Sample)
}

#' CPR Zoop genus product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooGenus()
#' @importFrom magrittr "%>%"
getCPRZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- getCPRZooData()

  cprZcl <- getCPRZooChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species
  cprGenZ1 <- cprZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::group_by(Sample, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(Genus != '')

  cprGenZ1 <- cprSamp %>%
    dplyr::left_join(cprGenZ1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Calanus", Genus),
                  ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenZ2 <- cprZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    dplyr::filter(Genus != '')  %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    dplyr::group_by(Sample, StartDate, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenZ2$Genus)) {
    Gen <- cprGenZ2 %>% dplyr::select(Genus) %>% unique()
    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Datesz <- as.data.frame(cprGenZ2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1) %>%
      droplevels()

    genz <- as.data.frame(cprGenZ2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    genz <- cprSamp %>%
      dplyr::left_join(genz, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Datesz$StartDate),
                    Genus = replace(Genus, is.na(Genus), Datesz$Genus),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateUTC, -999),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateUTC & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprGenZ1 <- dplyr::bind_rows(cprGenZ1, genz)
  }

  cprGenZ1 <- cprGenZ1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenZ <-  cprGenZ1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' CPR Zoop copepod product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooCopepod()
#' @importFrom magrittr "%>%"
getCPRZooCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- getCPRZooData()

  cprZcl <- getCPRZooChangeLog()%>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species

  cprCop1 <- cprZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprZcl$TaxonName)) &
                    Copepod =="COPEPOD" &
                    Species != "spp." &
                    !is.na(Species) &
                    Species != '' &
                    !grepl("cf.", Species) &
                    !grepl("/", Species) &
                    !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  cprCop1 <- cprSamp %>%
    dplyr::left_join(cprCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
                  ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>% # avoids nulls in pivot
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprCop2 <- cprZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & Copepod =="COPEPOD"
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)  &
                    !grepl("/", Species) & Species != '') %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    dplyr::mutate(Species = forcats::as_factor(Species)) %>% tidyr::drop_na(Species) %>%
    dplyr::group_by(Sample, StartDate, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprCop2$Species)) {
    Spe <- cprCop2 %>% dplyr::select(Species) %>% unique()
    Spe <- as.character(Spe$Species[i] %>% droplevels())

    Dates <- as.data.frame(cprCop2) %>%
      dplyr::filter(Species == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    copes <- as.data.frame(cprCop2) %>%
      dplyr::filter(Species == Spe) %>%
      droplevels()

    copes <- cprSamp %>%
      dplyr::left_join(copes, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Species = replace(Species, is.na(Species), Dates$Species),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateUTC, -999),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateUTC & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprCop1 <- dplyr::bind_rows(cprCop1, copes)
  }

  cprCop1 <- cprCop1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprCop <- cprCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Zoop copepod product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- getCPRZooNonCopepod()
#' @importFrom magrittr "%>%"
getCPRZooNonCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- getCPRZooData()

  cprZcl <- getCPRZooChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change logspecies
  cprnCop1 <- cprZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & Copepod !="COPEPOD"
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  cprnCop1 <- cprSamp %>%
    dplyr::left_join(cprnCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Evadne spinifera", Species),
                  ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprnCop2 <- cprZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & Copepod !="COPEPOD"
                  & Species != "spp." & Species != '' & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    tidyr::drop_na(Species) %>%
    dplyr::mutate(Species = forcats::as_factor(Species)) %>%
    dplyr::group_by(Sample, StartDate, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprnCop2$Species)) {
    Spe <- cprnCop2 %>% dplyr::select(Species) %>% unique()
    Spe <- as.character(Spe$Species[i] %>% droplevels())

    Dates <- as.data.frame(cprnCop2) %>%
      dplyr::filter(Species == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    ncopes <- as.data.frame(cprnCop2) %>%
      dplyr::filter(Species == Spe) %>%
      droplevels()

    ncopes <- cprSamp %>%
      dplyr::left_join(ncopes, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Species = replace(Species, is.na(Species), Dates$Species),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateUTC, -999),
                    ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateUTC & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprnCop1 <- dplyr::bind_rows(cprnCop1, ncopes)
  }

  cprnCop1 <- cprnCop1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>% as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprnCop <-  cprnCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#### NOW DO LARVAL FISH ####

#' Get Larval Fish Trip Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- getLFTrips()
#' #' @importFrom magrittr "%>%"
getLFTrips <- function(){
  LFSamp <- readr::read_csv(paste0(get_raw_plankton(), "BGC_LFish_Samples.csv"), na = "",
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    dplyr::rename(i_Sample = I_SAMPLE_ID, TripCode = TRIP_CODE, Station = STATIONNAME,
                  Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
                  ProjectName = PROJECTNAME, Volume_m3 = VOLUME_M3, Vessel = VESSEL,
                  TowType = TOWTYPE, GearDepth_m = GEARDEPTH_M, GearMesh_um = GEARMESH_UM,
                  WaterDepth_m = BATHYM_M, Temp_DegC = TEMPERATURE_C, Salinity = SALINITY,
                  Comments = COMMENTS, QC_Flag = QC_FLAG, FlagComments = FLAG_COMMENT) %>%
    dplyr::mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    dplyr::select(i_Sample:SampleDateLocal, Year:SampleDateLocal, Latitude:FlagComments)
}



#' Get Larval Fish Sample Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- getLFData()
#' #' @importFrom magrittr "%>%"
getLFData <- function(){
  LFData <- readr::read_csv(paste0(get_raw_plankton(), "BGC_LFish_CountRaw.csv"), na = "") %>%
    dplyr::rename(i_Sample = I_SAMPLE_ID, TripCode = TRIP_CODE,
                  ScientificName = SCIENTIFICNAME, SPCode = SPCODE,
                  Taxon_Count = TAXON_COUNT, Comments = COMMENTS)
  }


#' Make count data of all larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- getLFCountAll()
#' @importFrom magrittr "%>%"
getLFCountAll <- function(){

  LFCount <- getLFTrips() %>%
    dplyr::left_join(getLFData() %>%
                       dplyr::select(-Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    dplyr::select(-ScientificName, -SPCode) %>%
    dplyr::arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = Taxon_Count, values_fill = 0) %>%
    dplyr::arrange(SampleDateLocal)
}



#
#' Make BGC data for larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- getLFCountBGC()
#' @importFrom magrittr "%>%"
getLFCountBGC <- function(){
  LFCountBGC <- getLFTrips() %>%
    dplyr::filter(grepl('IMOS', ProjectName)) %>%
    dplyr::left_join(getLFData() %>%
                       dplyr::select(-Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    dplyr::select(-c(ScientificName, SPCode, Temp_DegC, Salinity)) %>%
    dplyr::arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = Taxon_Count, values_fill = 0) %>%
    dplyr::arrange(SampleDateLocal)
}

