
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
    dplyr::rename(TripCode = TRIP_CODE, Station = STATION, StationCode = STATIONCODE,
                  Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
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
}

# JDE adding this as it seems to be missed.

#' Get raw phytoplankton data in pivoted format
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSRawPhytoPivot()
#' #' @importFrom magrittr "%>%"
get_NRSRawPhytoPivot <- function(){

  NRSRawP <- dplyr::left_join(get_NRSTrips() %>% dplyr::filter(grepl('P', SampleType)),
                              get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCODE, SampleType)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#### Higher Trophic Groups Abund ####
#' Get Abundance of Phyto Higher Trophic Groups
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoHTG()
#' #' @importFrom magrittr "%>%"
get_NRSPhytoHTG <- function(){

  NRSHTGP <- get_NRSPhytoData()%>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP <- get_NRSTrips() %>%
    dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSHTGP, by = "TripCode") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::select(-c(SampleType))

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
}


#
#' Get NRS Phytoplankton raw pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoRaw()
#' @importFrom magrittr "%>%"
get_NRSPhytoRaw <- function(){
  NRSRawP <- dplyr::left_join(get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)),
                       get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCODE, SampleType)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#
#' Get NRS Phytoplankton higher taxon group pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoHTG()
#' @importFrom magrittr "%>%"
get_NRSPhytoHTG <- function(){
  NRSHTGP1 <- get_NRSPhytoData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP2 <- get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSHTGP1, by = "TripCode") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
           Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup))

  NRSHTGP <-  NRSHTGP2 %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::select(-c(SampleType))
}

#
#' Get NRS Phytoplankton genus pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoGenus()
#' @importFrom magrittr "%>%"
get_NRSPhytoGenus <- function() {
  # Bring in data once
  # These can be brought in from AODN once available
  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- get_NRSPhytoData()

  NRSPcl <- get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
           genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenP1 <- NRSPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != '') %>%
    dplyr::group_by(TripCode, Genus) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenP1 <- NRSSamp %>%
    dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSGenP1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
           Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenP2 <- NRSPdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != '') %>%
    dplyr::left_join(NRSPcl, by = "TaxonName") %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    dplyr::group_by(TripCode, StartDate, Genus) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen = NRSGenP2 %>%
      dplyr::select(Genus) %>% unique()
    Gen = as.character(Gen$Genus[i] %>% droplevels())
    Dates <- as.data.frame(NRSGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      dplyr::filter(grepl('P', SampleType)) %>%
      dplyr::left_join(gen, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             Genus = replace(Genus, is.na(Genus), Dates$Genus),
             Cells_L = replace(Cells_L, StartDate>SampleDateLocal, -999),
             Cells_L = replace(Cells_L, StartDate<SampleDateLocal & is.na(Cells_L), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSGenP1 <- dplyr::bind_rows(NRSGenP1, gen)
  }

  NRSGenP1 <- NRSGenP1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(Cells_L = max(Cells_L), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()

  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenP <-  NRSGenP1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal))  %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton species pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoSpecies()
#' @importFrom magrittr "%>%"
get_NRSPhytoSpecies <- function(){

  # Bring in data once
  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- get_NRSPhytoData()

  NRSPcl <- get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  # for non change log species

  NRSSpecP1 <- NRSPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or cilliates etc.
    dplyr::filter(!TaxonName %in% levels(as.factor(nrsls$TaxonName))
           & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("/", Species)) %>%
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  NRSSpecP1 <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSSpecP1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
           Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSSpecP2 <- NRSPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or cilliates etc.
    dplyr::filter(TaxonName %in% levels(as.factor(nrsls$TaxonName)) & TaxonName != '' &
             Species != "spp." & Species != "" & !is.na(Species) & !grepl("cf.", Species)) %>%
    dplyr::left_join(NRSPcl, by = "TaxonName") %>%
    dplyr::mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    dplyr::group_by(TripCode, StartDate, TaxonName) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecP2$TaxonName)) {
    Taxon = NRSSpecP2 %>% dplyr::select(TaxonName) %>% unique()
    Taxon = as.character(Taxon$TaxonName[i] %>% droplevels())
    Dates <- as.data.frame(NRSSpecP2) %>%
      dplyr::filter(TaxonName == Taxon) %>%
      dplyr::slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecP2) %>%
      dplyr::filter(TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
      dplyr::left_join(spec, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
             Cells_L = replace(Cells_L, StartDate>SampleDateLocal, -999),
             Cells_L = replace(Cells_L, StartDate<SampleDateLocal & is.na(Cells_L), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
      dplyr::summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSSpecP1 <- dplyr::bind_rows(NRSSpecP1, spec)
  }

  NRSSpecP1 <- NRSSpecP1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(Cells_L = max(Cells_L), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecP <-  NRSSpecP1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal))  %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}


#' Get NRS Phytoplankton raw pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoRawBV()
#' @importFrom magrittr "%>%"
get_NRSPhytoRawBV <- function(){
  NRSRawP <- dplyr::left_join(get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)),
                       get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Cells_L, SPCODE, SampleType)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Biovolume_um3L, values_fill = list(Biovolume_um3L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton higher taxon group pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoHTGBV()
#' @importFrom magrittr "%>%"
get_NRSPhytoHTGBV <- function() {
  NRSHTGPB1 <- get_NRSPhytoData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGPB1 <- get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSHTGPB1, by = "TripCode") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
           BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup))

  NRSHTGPB <-  NRSHTGPB1 %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::select(-c(TripCode, SampleType))
}

#' Get NRS Phytoplankton genus pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoGenusBV()
#' @importFrom magrittr "%>%"
get_NRSPhytoGenusBV <- function(){

  # Bring in all NRS phytoplankton samples, data and changelog once
  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- get_NRSPhytoData()

  NRSPcl <- get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
           genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenPB1 <- NRSPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != "") %>%
    dplyr::group_by(TripCode, Genus) %>%
    dplyr::summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenPB1 <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSGenPB1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
           BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenPB2 <- NRSPdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != "") %>%
    dplyr::left_join(NRSPcl, by = "TaxonName") %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    dplyr::group_by(TripCode, StartDate, Genus) %>%
    dplyr::summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenPB2$Genus)) {
    Gen = NRSGenPB2 %>% dplyr::select(Genus) %>% unique()
    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenPB2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenPB2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
      dplyr::left_join(gen, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             Genus = replace(Genus, is.na(Genus), Dates$Genus),
             BioV_um3L = replace(BioV_um3L, StartDate>SampleDateLocal, -999),
             BioV_um3L = replace(BioV_um3L, StartDate<SampleDateLocal & is.na(BioV_um3L), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
      as.data.frame()

    NRSGenPB1 <- dplyr::bind_rows(NRSGenPB1, gen)
  }

  NRSGenPB1 <- NRSGenPB1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(BioV_um3L = max(BioV_um3L), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()

  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenPB <-  NRSGenPB1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal))  %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton species pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_NRSPhytoSpeciesBV()
#' @importFrom magrittr "%>%"
get_NRSPhytoSpeciesBV <- function(){

  # Bring in needed data once
  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- get_NRSPhytoData()

  NRSPcl <- get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  # for non change log species

  NRSPdat1 <- NRSPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(!grepl("^(?!.*/~\\$)", TaxonName, perl = TRUE) &
             Species == "spp." &
             !TaxonName %in% levels(as.factor(nrsls$TaxonName)) )
  NRSSpecPB1 <- NRSPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrsls$TaxonName)) &
             Species != "spp." & !is.na(Species) & !grepl("cf.", Species)) %>%
    dplyr::bind_rows(NRSPdat1) %>%
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  NRSSpecPB1 <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
    dplyr::left_join(NRSSpecPB1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
           BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSPdat2 <- NRSPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(!grepl("^(?!.*/~\\$)", TaxonName, perl = TRUE) &
             Species == "spp." &
             TaxonName %in% levels(as.factor(nrsls$TaxonName)))

  NRSSpecPB2 <- NRSPdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrsls$TaxonName))
           & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & Species != "") %>%
    dplyr::bind_rows(NRSPdat2) %>%
    dplyr::left_join(NRSPcl, by = "TaxonName") %>%
    dplyr::filter(TaxonName != '') %>%
    dplyr::mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    dplyr::group_by(TripCode, StartDate, TaxonName) %>%
    dplyr::summarise(BioV_um3L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecPB2$TaxonName)) {
    Taxon = NRSSpecPB2 %>%
      dplyr::select(TaxonName) %>% unique()
    Taxon = as.character(Taxon$TaxonName[i] %>%
                           droplevels())

    Dates <- as.data.frame(NRSSpecPB2) %>%
      dplyr::filter(TaxonName == Taxon) %>%
      dplyr::slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecPB2) %>%
      dplyr::filter(TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>% dplyr::filter(grepl('P', SampleType)) %>%
      dplyr::left_join(spec, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
             BioV_um3L = replace(BioV_um3L, StartDate>SampleDateLocal, -999),
             BioV_um3L = replace(BioV_um3L, StartDate<SampleDateLocal & is.na(BioV_um3L), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
      dplyr::summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
      as.data.frame()
    NRSSpecPB1 <- dplyr::bind_rows(NRSSpecPB1, spec)
  }

  NRSSpecPB1 <- NRSSpecPB1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(BioV_um3L = max(BioV_um3L), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecPB <-  NRSSpecPB1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal))  %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
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
}


# Make NRS Zooplankton raw pivoted product
get_NRSZooRaw <- function(){
  NRSRawZ <- dplyr::left_join(get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('Z', SampleType)),
                       get_NRSZooData(), by = "TripCode") %>%
    dplyr::select(-c(Copepod, TaxonGroup, Genus, Species, SampleType, SPCODE)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
   tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton binned by sex and stage raw pivoted product
get_NRSZooRawBin <- function(){
  NRSIdsZ <- dplyr::left_join(get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>%
                                dplyr::filter(grepl('Z', SampleType)), get_NRSZooData(), by = "TripCode") %>%
    dplyr::mutate(TaxonName = ifelse(is.na(Genus), TaxonName, paste0(Genus, ' ', Species))) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, SampleDateUTC, TaxonName) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName))  %>%
   tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton higher taxonomic group product
get_NRSZooHTG <-  function(){
  nrsHTGZ1 <- get_NRSZooData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other"))

  nrsHTGZ1 <-  get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('Z', SampleType)) %>%
    dplyr::left_join(nrsHTGZ1, by = "TripCode") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Copepod", TaxonGroup),
           ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup))

  nrsHTGZ <-  nrsHTGZ1 %>%
   tidyr::pivot_wider(names_from = TaxonGroup, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::select(-c(SampleType)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton genus product
get_NRSZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- get_NRSZooData()

  NRSZcl <- get_NRSZooChangeLog()

  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  # Check genus are effected by change log
  nrszlg <- NRSZcl %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
           genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenZ1 <- NRSZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & Genus != "") %>%
    dplyr::group_by(TripCode, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenZ1 <- NRSSamp %>% dplyr::filter(grepl('Z', SampleType)) %>%
    dplyr::left_join(NRSGenZ1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = ifelse(is.na(Genus), "Acanthoica", stringr::word(Genus,1)), # bin subgenera together
           ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3))  %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenP2 <- NRSZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & Genus != "") %>%
    dplyr::left_join(NRSZcl, by = "TaxonName") %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    dplyr::group_by(TripCode, StartDate, Genus) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen <- NRSGenP2 %>%
      dplyr::select(Genus) %>%
      unique()

    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      dplyr::slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      dplyr::filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      dplyr::filter(grepl('Z', SampleType)) %>%
      dplyr::left_join(gen, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             Genus = replace(Genus, is.na(Genus), Dates$Genus),
             ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateLocal, -999),
             ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateLocal & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSGenZ1 <- dplyr::bind_rows(NRSGenZ1, gen)
  }

  NRSGenZ1 <- NRSGenZ1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSGenZ <- NRSGenZ1 %>%
   tidyr::pivot_wider(names_from = Genus, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton copepod product
get_NRSZooSpeciesCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- get_NRSZooData()

  NRSZcl <- get_NRSZooChangeLog()

  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  # for non change log species

  NRSCop1 <- NRSZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="COPEPOD"
           & Species != "spp." & !is.na(Species) & !grepl("cf.", Species)  &
             !grepl("/", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSCop1 <- NRSSamp %>%
    dplyr::filter(grepl('Z', SampleType)) %>%
    dplyr::left_join(NRSCop1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
           ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%  # avoids nulls in pivot
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSCop2 <- NRSZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="COPEPOD"
           & Species != "spp." & !is.na(Species)  & Species != "" &
             !grepl("/", Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::left_join(NRSZcl, by = "TaxonName") %>%
    dplyr::mutate(Species = forcats::as_factor(Species)) %>%
    tidyr::drop_na(Species) %>%
    dplyr::group_by(TripCode, StartDate, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSCop2$Species)) {
    Taxon <- NRSCop2 %>%
      dplyr::select(Species) %>% unique()
    Taxon <- as.character(Taxon$Species[i] %>% droplevels())

    Dates <- as.data.frame(NRSCop2) %>%
      dplyr::filter(Species == Taxon) %>%
      dplyr::slice(1) %>%
      droplevels()

    copes <- as.data.frame(NRSCop2) %>%
      dplyr::filter(Species == Taxon) %>%
      droplevels()

    copes <- NRSSamp %>%
      dplyr::filter(grepl('Z', SampleType)) %>%
      dplyr::left_join(copes, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             Species = replace(Species, is.na(Species), Dates$Species),
             ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateLocal, -999),
             ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateLocal & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSCop1 <- dplyr::bind_rows(NRSCop1, copes)
  }

  NRSCop1 <- NRSCop1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSCop <-  NRSCop1 %>%
   tidyr::pivot_wider(names_from = Species, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton non-copepod product
get_NRSZooSpeciesNonCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- get_NRSZooData()

  NRSZcl <- get_NRSZooChangeLog()

  NRSSamp <- get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  # for non change log species
  NRSnCop1 <- NRSZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="NON-COPEPOD"
           & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSnCop1 <- NRSSamp %>% dplyr::filter(grepl('Z', SampleType)) %>%
    dplyr::left_join(NRSnCop1, by = "TripCode") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
           ZAbund_m3 = ifelse(is.na(ZAbund_m3), 0, ZAbund_m3)) %>%  # avoids nulls in pivot
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSnCop2 <- NRSZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="NON-COPEPOD"  & Species != ""
           & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::left_join(NRSZcl, by = "TaxonName") %>%
    dplyr::mutate(Species = forcats::as_factor(Species)) %>%
    tidyr::drop_na(Species) %>%
    dplyr::group_by(TripCode, StartDate, Species) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSnCop2$Species)) {
    Taxon <- NRSnCop2 %>%
      dplyr::select(Species) %>%
      unique()
    Taxon <- as.character(Taxon$Species[i] %>% droplevels())

    Dates <- as.data.frame(NRSnCop2) %>%
      dplyr::filter(Species == Taxon) %>%
      dplyr::slice(1) %>%
      droplevels()

    ncopes <- as.data.frame(NRSnCop2) %>%
      dplyr::filter(Species == Taxon) %>%
      droplevels()

    ncopes <- NRSSamp %>% dplyr::filter(grepl('Z', SampleType)) %>%
      dplyr::left_join(ncopes, by = "TripCode") %>%
      dplyr::mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
             Species = replace(Species, is.na(Species), Dates$Species),
             ZAbund_m3 = replace(ZAbund_m3, StartDate>SampleDateLocal, -999),
             ZAbund_m3 = replace(ZAbund_m3, StartDate<SampleDateLocal & is.na(ZAbund_m3), 0)) %>%
      dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSnCop1 <- dplyr::bind_rows(NRSnCop1, ncopes)
  }

  NRSnCop1 <- NRSnCop1 %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZAbund_m3 = max(ZAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSnCop <- NRSnCop1 %>%
   tidyr::pivot_wider(names_from = Species, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
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
}

