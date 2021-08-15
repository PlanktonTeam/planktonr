#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_NRSStation()

pr_get_NRSStation <- function(){
  NRSStation <- readr::read_csv(paste0(pr_get_site(), "BGC_StationInfo.csv"), na = "") %>%
    pr_rename() %>%
    filter(ProjectName == "NRS")
  return(NRSStation)
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()

pr_get_NRSTrips <- function(){
  NRSSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_Trip.csv"), na = "") %>%
    pr_rename() %>%
    filter(ProjectName == "NRS") %>%
    mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast", warn = FALSE),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    select(TripCode:SampleDateLocal, Year:SampleDateUTC, Biomass_mgm3, Secchi_m, SampleType) %>%
    select(-tz)
}

#' Get raw phytoplankton data in pivoted format
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSRawPhytoPivot()
#' 
pr_get_NRSRawPhytoPivot <- function(){

  NRSRawP <- left_join(pr_get_NRSTrips() %>% filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCode, SampleType)) %>%
    arrange(-desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#### Higher Trophic Groups Abund ####
#' Get Abundance of Phyto Higher Trophic Groups
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTG()
#' 
pr_get_NRSPhytoHTG <- function(){

  NRSHTGP <- pr_get_NRSPhytoData()%>%
    group_by(TripCode, TaxonGroup) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP <- pr_get_NRSTrips() %>%
    filter(grepl('P', SampleType)) %>%
    left_join(NRSHTGP, by = "TripCode") %>%
    mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    arrange(-desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    select(-c(SampleType))

}



#' Import NRS Phytoplankton Data
#'
#' Load NRS station Phytoplankton Data
#' @return A dataframe with NRS Phytoplankton Data
#' @export
#' @examples
#' df <- pr_get_NRSPhytoData()

pr_get_NRSPhytoData <- function(){
  NRSPdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_Raw.csv"), na = "") %>%
    pr_rename()
}



#' Import NRS Phytoplankton Changelog
#'
#' Load NRS Phytoplankton Changelog
#' @return A dataframe with NRS Phytoplankton Changelog
#' @export
#' @examples
#' df <- pr_get_NRSPhytoChangeLog()

pr_get_NRSPhytoChangeLog <- function(){
  NRSPcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_ChangeLog.csv"), na = "") %>%
    pr_rename()
}


#
#' Get NRS Phytoplankton raw pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRaw()

pr_get_NRSPhytoRaw <- function(){
  NRSRawP <- left_join(pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>% filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    select(-c(TaxonGroup, Genus, Species, Biovolume_um3L, SPCode, SampleType)) %>%
    arrange(-desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#
#' Get NRS Phytoplankton higher taxon group pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTG()

pr_get_NRSPhytoHTG <- function(){
  NRSHTGP1 <- pr_get_NRSPhytoData() %>%
    group_by(TripCode, TaxonGroup) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP2 <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m)) %>%
    filter(grepl('P', SampleType)) %>%
    left_join(NRSHTGP1, by = "TripCode") %>%
    mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    arrange(-desc(TaxonGroup))

  NRSHTGP <-  NRSHTGP2 %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    select(-c(SampleType))
}

#
#' Get NRS Phytoplankton genus pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenus()

pr_get_NRSPhytoGenus <- function() {

  # Bring in data once
  # These can be brought in from AODN once available
  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenP1 <- NRSPdat %>%
    filter(!TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != '') %>%
    group_by(TripCode, Genus) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenP1 <- NRSSamp %>%
    filter(grepl('P', SampleType)) %>%
    left_join(NRSGenP1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
                  Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenP2 <- NRSPdat %>%
    filter(TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != '') %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    group_by(TripCode, StartDate, Genus) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen = NRSGenP2 %>%
      select(Genus) %>% unique()
    Gen = as.character(Gen$Genus[i] %>% droplevels())
    Dates <- as.data.frame(NRSGenP2) %>%
      filter(Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      filter(grepl('P', SampleType)) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Genus = replace(Genus, is.na(Genus), Dates$Genus),
                    Cells_L = replace(Cells_L, StartDate > SampleDateLocal, -999),
                    Cells_L = replace(Cells_L, StartDate < SampleDateLocal & is.na(Cells_L), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSGenP1 <- bind_rows(NRSGenP1, gen)
  }

  NRSGenP1 <- NRSGenP1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(Cells_L = max(Cells_L), .groups = "drop") %>%
    arrange(-desc(Genus)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenP <-  NRSGenP1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton species pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpecies()

pr_get_NRSPhytoSpecies <- function(){

  # Bring in data once
  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    filter(same == "no") # no changes at genera level

  # for non change log species

  NRSSpecP1 <- NRSPdat %>%
    mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or cilliates etc.
    filter(!TaxonName %in% levels(as.factor(nrsls$TaxonName))
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("/", Species)) %>%
    group_by(TripCode, TaxonName) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  NRSSpecP1 <- NRSSamp %>% filter(grepl('P', SampleType)) %>%
    left_join(NRSSpecP1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
                  Cells_L = ifelse(is.na(Cells_L), 0, Cells_L)) %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSSpecP2 <- NRSPdat %>%
    mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or cilliates etc.
    filter(TaxonName %in% levels(as.factor(nrsls$TaxonName)) & TaxonName != '' &
                    Species != "spp." & Species != "" & !is.na(Species) & !grepl("cf.", Species)) %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    group_by(TripCode, StartDate, TaxonName) %>%
    summarise(Cells_L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecP2$TaxonName)) {
    Taxon = NRSSpecP2 %>% select(TaxonName) %>% unique()
    Taxon = as.character(Taxon$TaxonName[i] %>% droplevels())
    Dates <- as.data.frame(NRSSpecP2) %>%
      filter(TaxonName == Taxon) %>%
      slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecP2) %>%
      filter(TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>% filter(grepl('P', SampleType)) %>%
      left_join(spec, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
                    Cells_L = replace(Cells_L, StartDate>SampleDateLocal, -999),
                    Cells_L = replace(Cells_L, StartDate<SampleDateLocal & is.na(Cells_L), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
      summarise(Cells_L = sum(Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSSpecP1 <- bind_rows(NRSSpecP1, spec)
  }

  NRSSpecP1 <- NRSSpecP1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    summarise(Cells_L = max(Cells_L), .groups = "drop") %>%
    arrange(-desc(TaxonName)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecP <-  NRSSpecP1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}


#' Get NRS Phytoplankton raw pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRawBV()

pr_get_NRSPhytoRawBV <- function(){
  NRSRawP <- left_join(pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>% filter(grepl('P', SampleType)),
                              pr_get_NRSPhytoData(), by = "TripCode") %>%
    select(-c(TaxonGroup, Genus, Species, Cells_L, SPCode, SampleType)) %>%
    arrange(-desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = Biovolume_um3L, values_fill = list(Biovolume_um3L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton higher taxon group pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTGBV()

pr_get_NRSPhytoHTGBV <- function() {
  NRSHTGPB1 <- pr_get_NRSPhytoData() %>%
    group_by(TripCode, TaxonGroup) %>%
    summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGPB1 <- pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>% filter(grepl('P', SampleType)) %>%
    left_join(NRSHTGPB1, by = "TripCode") %>%
    mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    arrange(-desc(TaxonGroup))

  NRSHTGPB <-  NRSHTGPB1 %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    select(-c(TripCode, SampleType))
}

#' Get NRS Phytoplankton genus pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenusBV()

pr_get_NRSPhytoGenusBV <- function(){

  # Bring in all NRS phytoplankton samples, data and changelog once
  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenPB1 <- NRSPdat %>%
    filter(!TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != "") %>%
    group_by(TripCode, Genus) %>%
    summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenPB1 <- NRSSamp %>%
    filter(grepl('P', SampleType)) %>%
    left_join(NRSGenPB1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
                  BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenPB2 <- NRSPdat %>%
    filter(TaxonName %in% levels(as.factor(nrslg$TaxonName)) & Genus != "") %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    group_by(TripCode, StartDate, Genus) %>%
    summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenPB2$Genus)) {
    Gen = NRSGenPB2 %>% select(Genus) %>% unique()
    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenPB2) %>%
      filter(Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenPB2) %>%
      filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>% filter(grepl('P', SampleType)) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Genus = replace(Genus, is.na(Genus), Dates$Genus),
                    BioV_um3L = replace(BioV_um3L, StartDate>SampleDateLocal, -999),
                    BioV_um3L = replace(BioV_um3L, StartDate<SampleDateLocal & is.na(BioV_um3L), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
      as.data.frame()

    NRSGenPB1 <- bind_rows(NRSGenPB1, gen)
  }

  NRSGenPB1 <- NRSGenPB1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(BioV_um3L = max(BioV_um3L), .groups = "drop") %>%
    arrange(-desc(Genus)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenPB <-  NRSGenPB1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

#' Get NRS Phytoplankton species pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpeciesBV()

pr_get_NRSPhytoSpeciesBV <- function(){

  # Bring in needed data once
  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    filter(same == "no") # no changes at genera level

  # for non change log species

  NRSPdat1 <- NRSPdat %>%
    mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    filter(!grepl("^(?!.*/~\\$)", TaxonName, perl = TRUE) &
                    Species == "spp." &
                    !TaxonName %in% levels(as.factor(nrsls$TaxonName)) )
  NRSSpecPB1 <- NRSPdat %>%
    filter(!TaxonName %in% levels(as.factor(nrsls$TaxonName)) &
                    Species != "spp." & !is.na(Species) & !grepl("cf.", Species)) %>%
    bind_rows(NRSPdat1) %>%
    group_by(TripCode, TaxonName) %>%
    summarise(BioV_um3L = sum(Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  NRSSpecPB1 <- NRSSamp %>% filter(grepl('P', SampleType)) %>%
    left_join(NRSSpecPB1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
                  BioV_um3L = ifelse(is.na(BioV_um3L), 0, BioV_um3L)) %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSPdat2 <- NRSPdat %>%
    mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    filter(!grepl("^(?!.*/~\\$)", TaxonName, perl = TRUE) &
                    Species == "spp." &
                    TaxonName %in% levels(as.factor(nrsls$TaxonName)))

  NRSSpecPB2 <- NRSPdat %>%
    filter(TaxonName %in% levels(as.factor(nrsls$TaxonName))
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & Species != "") %>%
    bind_rows(NRSPdat2) %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    filter(TaxonName != '') %>%
    mutate(TaxonName = forcats::as_factor(TaxonName)) %>%
    tidyr::drop_na(TaxonName) %>%
    group_by(TripCode, StartDate, TaxonName) %>%
    summarise(BioV_um3L = sum(Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecPB2$TaxonName)) {
    Taxon = NRSSpecPB2 %>%
      select(TaxonName) %>% unique()
    Taxon = as.character(Taxon$TaxonName[i] %>%
                           droplevels())

    Dates <- as.data.frame(NRSSpecPB2) %>%
      filter(TaxonName == Taxon) %>%
      slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecPB2) %>%
      filter(TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>% filter(grepl('P', SampleType)) %>%
      left_join(spec, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    TaxonName = replace(TaxonName, is.na(TaxonName), Dates$TaxonName),
                    BioV_um3L = replace(BioV_um3L, StartDate>SampleDateLocal, -999),
                    BioV_um3L = replace(BioV_um3L, StartDate<SampleDateLocal & is.na(BioV_um3L), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
      summarise(BioV_um3L = sum(BioV_um3L), .groups = "drop") %>%
      as.data.frame()
    NRSSpecPB1 <- bind_rows(NRSSpecPB1, spec)
  }

  NRSSpecPB1 <- NRSSpecPB1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, TaxonName) %>%
    summarise(BioV_um3L = max(BioV_um3L), .groups = "drop") %>%
    arrange(-desc(TaxonName)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecPB <-  NRSSpecPB1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}


#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooData()

pr_get_NRSZooData <- function(){
  NRSZdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_Raw.csv"), na = "") %>%
    pr_rename()
}

#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooCount()

pr_get_NRSZooCount <- function(){
  NRSZcount <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_Raw.csv"), na = "") %>%
    pr_rename()
}



#' Load zooplankton NRS Zooplankton changelog
#'
#' @return A dataframe with NRS zooplankton changelog
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooChangeLog()

pr_get_NRSZooChangeLog <- function(){
  NRSZcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_ChangeLog.csv"), na = "") %>%
    pr_rename()
}


# Make NRS Zooplankton raw pivoted product
pr_get_NRSZooRaw <- function(){
  NRSRawZ <- left_join(pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>% filter(grepl('Z', SampleType)),
                              pr_get_NRSZooData(), by = "TripCode") %>%
    select(-c(Copepod, TaxonGroup, Genus, Species, SampleType, SPCode)) %>%
    arrange(-desc(TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton binned by sex and stage raw pivoted product
pr_get_NRSZooRawBin <- function(){
  NRSIdsZ <- left_join(pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>%
                                filter(grepl('Z', SampleType)), pr_get_NRSZooData(), by = "TripCode") %>%
    mutate(TaxonName = ifelse(is.na(Genus), TaxonName, paste0(Genus, ' ', Species))) %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, SampleDateUTC, TaxonName) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE)) %>%
    arrange(-desc(TaxonName))  %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton higher taxonomic group product
pr_get_NRSZooHTG <-  function(){
  nrsHTGZ1 <- pr_get_NRSZooData() %>%
    group_by(TripCode, TaxonGroup) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    filter(!TaxonGroup %in% c("Other"))

  nrsHTGZ1 <-  pr_get_NRSTrips() %>% select(-c(Biomass_mgm3, Secchi_m)) %>% filter(grepl('Z', SampleType)) %>%
    left_join(nrsHTGZ1, by = "TripCode") %>%
    mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Copepod", TaxonGroup),
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%
    arrange(-desc(TaxonGroup))

  nrsHTGZ <-  nrsHTGZ1 %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    select(-c(SampleType)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton genus product
pr_get_NRSZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  # Check genus are effected by change log
  nrszlg <- NRSZcl %>%
    mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    filter(same == "no")# no changes at genera level

  # for non change log species
  NRSGenZ1 <- NRSZdat %>%
    filter(!TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & Genus != "") %>%
    group_by(TripCode, Genus) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(Genus)

  NRSGenZ1 <- NRSSamp %>% filter(grepl('Z', SampleType)) %>%
    left_join(NRSGenZ1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", stringr::word(Genus,1)), # bin subgenera together
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3))  %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenP2 <- NRSZdat %>%
    filter(TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & Genus != "") %>%
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    group_by(TripCode, StartDate, Genus) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen <- NRSGenP2 %>%
      select(Genus) %>%
      unique()

    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenP2) %>%
      filter(Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      filter(Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      filter(grepl('Z', SampleType)) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Genus = replace(Genus, is.na(Genus), Dates$Genus),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateLocal, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateLocal & is.na(ZooPhytoAbund_m3), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
      summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSGenZ1 <- bind_rows(NRSGenZ1, gen)
  }

  NRSGenZ1 <- NRSGenZ1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Genus) %>%
    summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    arrange(-desc(Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSGenZ <- NRSGenZ1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton copepod product
pr_get_NRSZooSpeciesCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    filter(same == "no") # no changes at genera level

  # for non change log species

  NRSCop1 <- NRSZdat %>%
    filter(!TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="COPEPOD"
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species)  &
                    !grepl("/", Species) & !grepl("grp", Species)) %>%
    mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSCop1 <- NRSSamp %>%
    filter(grepl('Z', SampleType)) %>%
    left_join(NRSCop1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%  # avoids nulls in pivot
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSCop2 <- NRSZdat %>%
    filter(TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="COPEPOD"
                  & Species != "spp." & !is.na(Species)  & Species != "" &
                    !grepl("/", Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Species = forcats::as_factor(Species)) %>%
    tidyr::drop_na(Species) %>%
    group_by(TripCode, StartDate, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSCop2$Species)) {
    Taxon <- NRSCop2 %>%
      select(Species) %>% unique()
    Taxon <- as.character(Taxon$Species[i] %>% droplevels())

    Dates <- as.data.frame(NRSCop2) %>%
      filter(Species == Taxon) %>%
      slice(1) %>%
      droplevels()

    copes <- as.data.frame(NRSCop2) %>%
      filter(Species == Taxon) %>%
      droplevels()

    copes <- NRSSamp %>%
      filter(grepl('Z', SampleType)) %>%
      left_join(copes, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Species = replace(Species, is.na(Species), Dates$Species),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateLocal, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateLocal & is.na(ZooPhytoAbund_m3), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
      summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSCop1 <- bind_rows(NRSCop1, copes)
  }

  NRSCop1 <- NRSCop1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    arrange(-desc(Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSCop <-  NRSCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
}

# Make NRS Zooplankton non-copepod product
pr_get_NRSZooSpeciesNonCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips() %>%
    select(-c(Biomass_mgm3, Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    filter(same == "no") # no changes at genera level

  # for non change log species
  NRSnCop1 <- NRSZdat %>%
    filter(!TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="NON-COPEPOD"
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSnCop1 <- NRSSamp %>% filter(grepl('Z', SampleType)) %>%
    left_join(NRSnCop1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%  # avoids nulls in pivot
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSnCop2 <- NRSZdat %>%
    filter(TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & Copepod =="NON-COPEPOD"  & Species != ""
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Species = forcats::as_factor(Species)) %>%
    tidyr::drop_na(Species) %>%
    group_by(TripCode, StartDate, Species) %>%
    summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSnCop2$Species)) {
    Taxon <- NRSnCop2 %>%
      select(Species) %>%
      unique()
    Taxon <- as.character(Taxon$Species[i] %>% droplevels())

    Dates <- as.data.frame(NRSnCop2) %>%
      filter(Species == Taxon) %>%
      slice(1) %>%
      droplevels()

    ncopes <- as.data.frame(NRSnCop2) %>%
      filter(Species == Taxon) %>%
      droplevels()

    ncopes <- NRSSamp %>% filter(grepl('Z', SampleType)) %>%
      left_join(ncopes, by = "TripCode") %>%
      mutate(StartDate = replace(StartDate, is.na(StartDate), Dates$StartDate),
                    Species = replace(Species, is.na(Species), Dates$Species),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateLocal, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateLocal & is.na(ZooPhytoAbund_m3), 0)) %>%
      group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
      summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSnCop1 <- bind_rows(NRSnCop1, ncopes)
  }

  NRSnCop1 <- NRSnCop1 %>%
    group_by(TripCode, StationName, Latitude, Longitude, SampleDateLocal, Year, Month, Day, Time_24hr, Species) %>%
    summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    arrange(-desc(Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSnCop <- NRSnCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    arrange(desc(SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(SampleDateLocal))
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
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()

pr_get_NRSPico <- function(){
  Pico <- readr::read_csv(paste0(pr_get_site(), "BGC_Picoplankton.csv"), na = "") %>%
    pr_rename()
}

