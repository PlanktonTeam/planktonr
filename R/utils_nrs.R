#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_NRSStation()
#' @importFrom magrittr "%>%"
pr_get_NRSStation <- function(){
  NRSStation <- readr::read_csv(paste0(pr_get_site(), "BGC_StationInfo.csv"), na = "") %>%
    pr_rename() %>%
    # dplyr::rename(Station = STATIONNAME, Latitude = LATITUDE, Longitude = LONGITUDE, StationDepth_m = STATIONDEPTH_M) %>%
    dplyr::filter(ProjectName == "NRS")
  return(NRSStation)
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
#' @importFrom magrittr "%>%"
pr_get_NRSTrips <- function(){
  NRSSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_Trip.csv"), na = "") %>%
    pr_rename() %>%
    # dplyr::rename(TripCode = TRIP_CODE, Station = STATIONNAME, StationCode = STATIONCODE,
    #               Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
    #               Biomass_mgm3 = BIOMASS_MGM3, Secchi_m = SECCHI_M, SampleType = SAMPLETYPE,
    #               ZoopSampleDepth_m = ZOOPSAMPLEDEPTH_M, PhytoSampleDepth_m = PHYTOSAMPLEDEPTH_M) %>%
    dplyr::filter(ProjectName == "NRS") %>%
    dplyr::mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    dplyr::select(TripCode:SampleDateLocal, Year:SampleDateUTC, Biomass_mgm3, Secchi_m, SampleType) %>%
    dplyr::select(-tz)
}

#' Get raw phytoplankton data in pivoted format
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSRawPhytoPivot()
#' #' @importFrom magrittr "%>%"
pr_get_NRSRawPhytoPivot <- function(){

  NRSRawP <- dplyr::left_join(pr_get_NRSTrips() %>% dplyr::filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCode, SampleType)) %>%
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
#' df <- pr_get_NRSPhytoHTG()
#' #' @importFrom magrittr "%>%"
pr_get_NRSPhytoHTG <- function(){

  NRSHTGP <- pr_get_NRSPhytoData()%>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP <- pr_get_NRSTrips() %>%
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
#' df <- pr_get_NRSPhytoData()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoData <- function(){
  NRSPdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_Raw.csv"), na = "") %>%
    pr_rename()
  # dplyr::rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, TaxonGroup = TAXON_GROUP, Genus = GENUS, Species = SPECIES,
  #               Cells_L = CELL_L, Biovolume_um3L = BIOVOLUME_UM3L)
}



#' Import NRS Phytoplankton Changelog
#'
#' Load NRS Phytoplankton Changelog
#' @return A dataframe with NRS Phytoplankton Changelog
#' @export
#' @examples
#' df <- pr_get_NRSPhytoChangeLog()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoChangeLog <- function(){
  NRSPcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_ChangeLog.csv"), na = "") %>%
    pr_rename()
  # dplyr::rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
}


#
#' Get NRS Phytoplankton raw pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRaw()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoRaw <- function(){
  NRSRawP <- dplyr::left_join(pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCode, SampleType)) %>%
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
#' df <- pr_get_NRSPhytoHTG()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoHTG <- function(){
  NRSHTGP1 <- pr_get_NRSPhytoData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP2 <- pr_get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>%
    dplyr::filter(grepl('P', SampleType)) %>%
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
#' df <- pr_get_NRSPhytoGenus()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoGenus <- function() {

  # Bring in data once
  # These can be brought in from AODN once available
  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

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
                    Cells_L = replace(Cells_L, StartDate > SampleDateLocal, -999),
                    Cells_L = replace(Cells_L, StartDate < SampleDateLocal & is.na(Cells_L), 0)) %>%
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
#' df <- pr_get_NRSPhytoSpecies()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoSpecies <- function(){

  # Bring in data once
  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

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
#' df <- pr_get_NRSPhytoRawBV()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoRawBV <- function(){
  NRSRawP <- dplyr::left_join(pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    dplyr::select(-c(TaxonGroup, Genus, Species, Cells_L, SPCode, SampleType)) %>%
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
#' df <- pr_get_NRSPhytoHTGBV()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoHTGBV <- function() {
  NRSHTGPB1 <- pr_get_NRSPhytoData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGPB1 <- pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('P', SampleType)) %>%
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
#' df <- pr_get_NRSPhytoGenusBV()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoGenusBV <- function(){

  # Bring in all NRS phytoplankton samples, data and changelog once
  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

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

  NRSGenPB1 <- NRSSamp %>%
    dplyr::filter(grepl('P', SampleType)) %>%
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
#' df <- pr_get_NRSPhytoSpeciesBV()
#' @importFrom magrittr "%>%"
pr_get_NRSPhytoSpeciesBV <- function(){

  # Bring in needed data once
  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

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
#' df <- pr_get_NRSZooData()
#' @importFrom magrittr "%>%"
pr_get_NRSZooData <- function(){
  NRSZdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_Raw.csv"), na = "") %>%
    pr_rename()
  # dplyr::rename(TripCode = TRIP_CODE, TaxonName = TAXON_NAME, Copepod = COPEPOD, TaxonGroup = TAXON_GROUP,
  #               Genus = GENUS, Species = SPECIES, ZAbund_m3 = ZOOP_ABUNDANCE_M3, TaxonCount = TAXON_COUNT, SampVol_m3 = SAMPVOL_M3)
}

#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooCount()
#' @importFrom magrittr "%>%"
pr_get_NRSZooCount <- function(){
  NRSZcount <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_Raw.csv"), na = "") %>%
    pr_rename()
  # dplyr::rename(TaxonName = TAXON_NAME, Copepod = COPEPOD, TaxonGroup = TAXON_GROUP, TripCode = TRIP_CODE,
  # Genus = GENUS, Species = SPECIES, TaxonCount = TAXON_COUNT, SampVol_m3 = SAMPVOL_M3)
}



#' Load zooplankton NRS Zooplankton changelog
#'
#' @return A dataframe with NRS zooplankton changelog
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooChangeLog()
#' @importFrom magrittr "%>%"
pr_get_NRSZooChangeLog <- function(){
  NRSZcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_ChangeLog.csv"), na = "") %>%
    pr_rename()
  # dplyr::rename(TaxonName = TAXON_NAME, StartDate = START_DATE, ParentName = PARENT_NAME)
}


# Make NRS Zooplankton raw pivoted product
pr_get_NRSZooRaw <- function(){
  NRSRawZ <- dplyr::left_join(pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('Z', SampleType)),
                              pr_get_NRSZooData(), by = "TripCode") %>%
    dplyr::select(-c(Copepod, TaxonGroup, Genus, Species, SampleType, SPCode)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton binned by sex and stage raw pivoted product
pr_get_NRSZooRawBin <- function(){
  NRSIdsZ <- dplyr::left_join(pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>%
                                dplyr::filter(grepl('Z', SampleType)), pr_get_NRSZooData(), by = "TripCode") %>%
    dplyr::mutate(TaxonName = ifelse(is.na(Genus), TaxonName, paste0(Genus, ' ', Species))) %>%
    dplyr::group_by(TripCode, Station, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, SampleDateUTC, TaxonName) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName))  %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZAbund_m3, values_fill = list(ZAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateLocal)) %>%
    dplyr::mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton higher taxonomic group product
pr_get_NRSZooHTG <-  function(){
  nrsHTGZ1 <- pr_get_NRSZooData() %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(ZAbund_m3 = sum(ZAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other"))

  nrsHTGZ1 <-  pr_get_NRSTrips() %>% dplyr::select(-c(Biomass_mgm3, Secchi_m)) %>% dplyr::filter(grepl('Z', SampleType)) %>%
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
pr_get_NRSZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
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
pr_get_NRSZooSpeciesCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
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
pr_get_NRSZooSpeciesNonCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
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
#' df <- pr_get_NRSPigments()
pr_get_NRSPigments <- function(){
  Pigments <- readr::read_csv(paste0(pr_get_site(),"BGC_Pigments.csv"), na = "") %>%
    pr_rename()
    # dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, PigmentsFlag = PIGMENTS_FLAG, PigmentsComments = PIGMENTS_COMMENTS)
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()
#' @importFrom magrittr "%>%"
pr_get_NRSPico <- function(){
  Pico <- readr::read_csv(paste0(pr_get_site(), "BGC_Picoplankton.csv"), na = "") %>%
    pr_rename()
    # dplyr::rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, Replicate = REPLICATE, SampleDateLocal = SAMPLEDATELOCAL,
    #               Prochlorococcus_Cellsml = PROCHLOROCOCCUS_CELLSML, Prochlorococcus_Flag = PROCHLOROCOCCUS_FLAG,
    #               Synecochoccus_Cellsml = SYNECOCHOCCUS_CELLSML, Synecochoccus_Flag = SYNECOCHOCCUS_FLAG,
    #               Picoeukaryotes_Cellsml = PICOEUKARYOTES_CELLSML, Picoeukaryotes_Flag = PICOEUKARYOTES_FLAG)
}

