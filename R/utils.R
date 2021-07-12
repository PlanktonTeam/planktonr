
# Testing
# pkgload::load_all()
# devtools::check()


# ## Functions for operating
# untibble <- function (tibble) {
#   data.frame(unclass(tibble), check.names = FALSE, stringsAsFactors = FALSE)
# }  ## escape the nonsense
#
# raw <- "https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/"
# output <- "https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/"

## NRS FUNCTIONS
## Functions for bringing in data sets

#' Function for importing NRS Station information
#' @importFrom magrittr "%>%"
get_NRSStation <- function(){
  # requireNamespace("magrittr", quietly = TRUE)
  NRSStation <- readr::read_csv(paste0(raw, "BGC_StationInfo.csv"), na = "") %>%
    dplyr::rename(Station = STATIONNAME, Latitude = LATITUDE, Longitude = LONGITUDE, StationDepth_m = STATIONDEPTH_m) %>%
    dplyr::filter(PROJECTNAME == "NRS")
  return(NRSStation)
}
#
# # Bring in all NRS samples
# getNRSTrips <- function(){
#   NRSSamp <- read_csv(paste0(raw, "BGC_Trip.csv"), na = "") %>%
#     rename(TripCode = TRIP_CODE, Station = STATION, StationCode = STATIONCODE, Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
#            Biomass_mgm3 = BIOMASS_MGM3, Secchi_m = SECCHI_M, SampleType = SAMPLETYPE) %>%
#     filter(PROJECTNAME == "NRS") %>%
#     mutate(Year = year(SampleDateLocal),
#            Month = month(SampleDateLocal),
#            Day = day(SampleDateLocal),
#            Time_24hr = str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
#            tz = tz_lookup_coords(Latitude, Longitude, method = "fast"),
#            SampleDateUTC = with_tz(force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
#     select(TripCode:SampleDateLocal, Year:SampleDateUTC, Biomass_mgm3, Secchi_m, SampleType) %>%
#     select(-tz)
#   return(NRSSamp)
# }
#
# # Bring in plankton data
# getNRSPhytoData <- function(){
#   NRSPdat <- read_csv(paste0(raw, "BGC_Phyto_Raw.csv"), na = "") %>%
#     rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, TaxonGroup = TAXON_GROUP, Genus = GENUS, Species = SPECIES,
#            Cells_L = CELL_L, Biovolume_um3L = BIOVOLUME_UM3L)
#   return(NRSPdat)
# }
#
# # Bring in Change Log
# getNRSPhytoChangeLog <- function(){
#   NRSPcl <- read_csv(paste0(raw, "BGC_Phyto_ChangeLog.csv"), na = "") %>%
#     rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
#   return(NRSPcl)
# }
#
# # Bring in zooplankton  abundance data
# getNRSZooData <- function(){
#   NRSZdat <- read_csv(paste0(raw, "BGC_Zoop_Raw.csv"), na = "") %>%
#     rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01,
#            Genus = GENUS, Species = SPECIES, ZAbund_m3 = ZOOP_ABUNDANCE_M3)
#   return(NRSZdat)
# }
#
# # Bring in zooplankton  abundance data
# getNRSZooCount <- function(){
#   NRSZcount <- read_csv(paste0(raw, "BGC_Zoop_CountRaw.csv"), na = "") %>%
#     rename(TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01, TripCode = TRIP_CODE,
#            Genus = GENUS, Species = SPECIES, TaxonCount = COUNTS, SampVol_L = SAMPVOL_L)
#   return(NRSZcount)
# }
#
# # Bring in Change Log
# getNRSZooChangeLog <- function(){
#   NRSZcl <- read_csv(paste0(raw, "BGC_Zoop_ChangeLog.csv"), na = "") %>%
#     rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
#   return(NRSZcl)
# }
#
# # Bring in copepod information table with sizes etc.
# get_ZooInfo <- function(){
#   ZInfo <- read_csv(paste0(raw, "taxon_info.csv"), na = "",
#                     col_types = cols(SIZE_MIN_MM = col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
#                                      SIZE_MAX_MM = col_double(),
#                                      SIZE_AVE_MM = col_double())) %>%
#     rename( "TaxonName" = "TAXON_NAME") %>%
#     untibble()
# }
#
#
# # Bring in chemistry data
# getChemistry <- function(){
#   chemistry <- read_csv(paste0(raw, "BGC_Chemistry.csv"), na = c("", NaN),
#                         col_types = cols(DIC_UMOLKG = col_double(),
#                                          OXYGEN_UMOLL = col_double(),
#                                          OXYGEN_COMMENTS = col_character())) %>%
#     rename(TripCode = TRIP_CODE,
#            SampleDepth_m = SAMPLEDEPTH_M, Silicate_umolL = SILICATE_UMOLL, Nitrate_umolL =  NITRATE_UMOLL,
#            Phosphate_umolL =  PHOSPHATE_UMOLL, Salinity_PSU = SALINITY_PSU,
#            Ammonium_umolL =  AMMONIUM_UMOLL,
#            Nitrite_umolL =  NITRITE_UMOLL,
#            DIC_umolkg =  DIC_UMOLKG,
#            TAlkalinity_umolkg =  TALKALINITY_UMOLKG,
#            Oxygen_umolL =  OXYGEN_UMOLL) %>%
#     mutate(SampleDepth_m = as.character(SampleDepth_m),
#            Silicate_umolL = ifelse(SILICATE_FLAG %in% c(3,4,9), NA, Silicate_umolL), # remove all data flagged as bad or probably bad
#            Phosphate_umolL = ifelse(PHOSPHATE_FLAG %in% c(3,4,9), NA, Phosphate_umolL),
#            Ammonium_umolL = ifelse(AMMONIUM_FLAG %in% c(3,4,9), NA, Ammonium_umolL),
#            Nitrate_umolL = ifelse(NITRATE_FLAG %in% c(3,4,9), NA, Nitrate_umolL),
#            Nitrite_umolL = ifelse(NITRITE_FLAG %in% c(3,4,9), NA, Nitrite_umolL),
#            Oxygen_umolL = ifelse(OXYGEN_FLAG %in% c(3,4,9), NA, Oxygen_umolL),
#            DIC_umolkg = ifelse(CARBON_FLAG %in% c(3,4,9), NA, DIC_umolkg),
#            TAlkalinity_umolkg = ifelse(ALKALINITY_FLAG %in% c(3,4,9), NA, TAlkalinity_umolkg),
#            Salinity_PSU = ifelse(SALINITY_FLAG %in% c(3,4,9), NA, Salinity_PSU)) %>%
#     group_by(TripCode, SampleDepth_m) %>%
#     summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE), # some replicated samples from error picking up PHB data, needs addressing in database
#               Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
#               Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
#               Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
#               Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
#               Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
#               DIC_umolkg = sum(DIC_umolkg, na.rm = TRUE),
#               TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
#               Salinity_PSU = mean(Salinity_PSU, na.rm = TRUE),
#               .groups = "drop") %>%
#     ungroup() %>%
#     mutate_all(~ replace(., is.na(.), NA)) %>%
#     untibble()
#   return(chemistry)
# }
#
# # Bring in picoplankton data
# getPico <- function(){
#   Pico <- read_csv(paste0(raw, "BGC_Picoplankton.csv"), na = "") %>%
#     rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, Replicate = REPLICATE, SampleDate_Local = SAMPLEDATE_LOCAL,
#            Prochlorococcus_CellsmL = PROCHLOROCOCCUS_CELLSML, Prochlorococcus_Flag = PROCHLOROCOCCUS_FLAG,
#            Synecochoccus_CellsmL = SYNECOCHOCCUS_CELLSML, Synecochoccus_Flag = SYNECOCHOCCUS_FLAG,
#            Picoeukaryotes_CellsmL = PICOEUKARYOTES_CELLSML, Picoeukaryotes_Flag = PICOEUKARYOTES_FLAG)
#   return(Pico)
# }
# # get CTD data
#
# ## make raw product that should be similar to that produced by AODN
# getCTD <- function(){
#   rawCTD <- read_csv(paste0(raw, "IMOS_-_Australian_National_Mooring_Network_(ANMN)_-_CTD_Profiles.csv"), na = "", skip = 29,
#                      col_types = cols(CHLU = col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
#                                       CHLU_quality_control = col_double(),
#                                       CPHL = col_double(),
#                                       CPHL_quality_control = col_double(),
#                                       cruise_id = col_skip())) %>%
#     filter(grepl("NRS", site_code)) %>%
#     mutate(TripCode = ifelse(site_code == 'NRSDAR', paste0(site_code, format(time_coverage_start, "%Y%m%d_%H:%M")),
#                              paste0(site_code, format(time_coverage_start, "%Y%m%d"))),
#            StationName = ifelse(site_code == 'NRSDAR', 'Darwin',
#                                 ifelse(site_code == 'NRSYON', 'Yongala',
#                                        ifelse(site_code == 'NRSNSI', 'North Stradbroke Island',
#                                               ifelse(site_code == 'NRSPHB', 'Port Hacking',
#                                                      ifelse(site_code == 'NRSMAI', 'Maria Island',
#                                                             ifelse(site_code == 'NRSKAI', 'Kangaroo Island',
#                                                                    ifelse(site_code == 'NRSESP', 'Esperance',
#                                                                           ifelse(site_code == 'NRSROT', 'Rottnest Island', 'Ningaloo')))))))),
#            CPHL = ifelse(!is.na(CPHL), CPHL, CHLF)) %>%
#     rename(CastTime_UTC = time_coverage_start, Latitude = LATITUDE, Longitude = LONGITUDE, Depth_m = DEPTH, Salinity_psu = PSAL,
#            Salinity_flag = PSAL_quality_control, Temperature_degC = TEMP, Temperature_flag = TEMP_quality_control, DissolvedOxygen_umolkg = DOX2,
#            DissolvedOxygen_flag = DOX2_quality_control, Chla_mgm3 = CPHL, Chla_flag = CPHL_quality_control, Turbidity_NTU = TURB,
#            Turbidity_flag = TURB_quality_control, Pressure_dbar = PRES_REL, Conductivity_Sm = CNDC, Conductivity_flag = CNDC_quality_control,
#            WaterDensity_kgm3 = DENS, WaterDensity_flag = DENS_quality_control) %>%
#     select(file_id, StationName, TripCode, CastTime_UTC, Latitude, Longitude, Depth_m, Salinity_psu, Salinity_flag, Temperature_degC, Temperature_flag,
#            DissolvedOxygen_umolkg, DissolvedOxygen_flag, Chla_mgm3, Chla_flag, Turbidity_NTU, Turbidity_flag, Pressure_dbar, Conductivity_Sm,
#            Conductivity_flag, WaterDensity_kgm3, WaterDensity_flag) %>%
#     mutate(tz = paste("\"", tz_lookup_coords(Latitude, Longitude, method = "fast"),"\""),
#            castTime_Local = with_tz(CastTime_UTC, tz = "Australia/Perth")) %>%
#     filter(!file_id %in% c(2117, 2184, 2186, 2187))
#
#   NRSSamp <- getNRSTrips() %>% filter(!grepl('PH4', TripCode))
#
#   Stations <- NRSSamp %>% select(TripCode) %>% mutate(stations = as.factor(substr(TripCode, 1, 3))) %>% select(stations) %>% unique()
#   df <- data.frame(file_id = NA, TripCode = NA)
#
#   for (y in 1:nlevels(Stations$stations)){
#     station  <-  levels(Stations$stations)[[y]]
#     rawCTDCast <- rawCTD %>% select(file_id, CastTime_UTC, TripCode) %>% filter(substr(TripCode, 4, 6) == station) %>% unique()
#     CastTimes <- rawCTDCast$CastTime_UTC
#     Samps <- NRSSamp %>% filter(substr(TripCode, 1, 3) == station) %>% select(SampleDateUTC, TripCode) %>% unique()
#
#     dateSelect <- function(x){
#       which.min(abs(x - CastTimes))
#     }
#
#     DateMatch <- sapply(Samps$SampleDateUTC, dateSelect)
#     Samps$SampLevel <-  DateMatch
#     Samps$CastTime_UTC <- Samps$SampleDateUTC
#
#     for (i in 1:nrow(Samps)){
#       j <- Samps$SampLevel[[i]]
#       Samps$CastTime_UTC[i] <- CastTimes[[j]]
#     }
#
#     Samps <- Samps %>% mutate(DateDiff = abs(CastTime_UTC - SampleDateUTC) / 3600,
#                               DateDiff = ifelse(DateDiff > 3 & station != 'NSI', NA,
#                                                 ifelse(DateDiff > 15 & station %in% c('NSI', 'KAI'), NA, DateDiff)))
#
#     SampsMatch <- rawCTDCast %>% filter(substr(TripCode, 4, 6) == station) %>% select(CastTime_UTC, file_id) %>% unique()
#
#     CastMatch <- Samps %>% drop_na(DateDiff) %>% inner_join(SampsMatch, by = 'CastTime_UTC') %>% select(file_id, TripCode)
#
#     df <- df %>% rbind(CastMatch)
#   }
#
#   rawCTD <- rawCTD %>% select(-TripCode) %>% left_join(df, by = 'file_id')
#
#   return(rawCTD)
# }
#
# #CTD <- getCTD() %>% drop_na(TripCode)
# #write_csv(CTD, "RawData/NRS_CTD.csv")
#
# #
# # missingCode <- rawCTD %>% filter(is.na(TripCode)) %>% select(StationName, CastTime_UTC, file_id) %>% unique()
# #
# # NRSaddCTD <- NRSSamp %>% left_join(df, by = 'TripCode')
# # NRSmissingCTD <- NRSaddCTD %>% filter(is.na(file_id))
#
# # getCTD <- function(){
# #   CTD <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/nrs_ctd.csv", na = "",
# #                 col_types = cols(PRES = col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
# #                                  PAR = col_double(),
# #                                  SPEC_CNDC = col_double())) %>%
# #   rename(TripCode = NRS_TRIP_CODE, SampleDepth_m = PRES_REL, CTDDensity_kgm3 = DENS,
# #          CTDTemperature = TEMP, CTDPAR_umolm2s = PAR,
# #          CTDConductivity_sm = CNDC, CTDSpecificConductivity_Sm = SPEC_CNDC,
# #          CTDSalinity = PSAL, CTDTurbidity_ntu = TURB, CTDChlF_mgm3 = CHLF) %>%
# #   untibble()
# #
# #   CTD_remove <- CTD %>% group_by(TripCode) %>% summarise(n = n()) %>%
# #     filter(n == 1)
# #   CTD <- CTD %>% filter(!TripCode %in% CTD_remove$TripCode) # removing records where CTD length is one value
# #
# #   return(CTD)
# # }
#
# ############################################################################################################################################
# ## CPR FUNCTIONS
# # Bring in all CPR samples
#
# # All CPR samples
#
# getCPRTrips <- function(){
#   CPRTrips <- read_csv(paste0(raw, "CPR_Samp.csv"), na = "(null)") %>%
#     rename(Sample = SAMPLE, Route = ROUTE, Region = REGION, Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateUTC = SAMPLEDATEUTC,
#            SampleType = SAMPLETYPE, Biomass_mgm3 = BIOMASS_MGM3, TripCode = TRIP_CODE) %>%
#     filter(!is.na(SampleType)) %>%
#     mutate(Year = year(SampleDateUTC),
#            Month = month(SampleDateUTC),
#            Day = day(SampleDateUTC),
#            Time_24hr = str_sub(SampleDateUTC, -8, -1)) %>%
#     select(c(TripCode, Sample, Latitude:Time_24hr, Region, Route, PCI, Biomass_mgm3))
#   return(CPRTrips)
# }
#
# # CPR Zooplankton Count
#
# getCPRZooCount <- function() {
#   CPRZooCount <- read_csv(paste0(raw, "CPR_Zoop_CountRaw.csv"), na = "(null)") %>%
#     rename(TaxonName = TAXON_NAME, Copepod = TAXON_GROUP, TaxonGroup = TAXON_GRP01, Sample = SAMPLE,
#            Genus= GENUS, Species = SPECIES, TaxonCount = COUNTS, SampVol_L = SAMPVOL_L)
#   return(CPRZooCount)
# }

