#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_NRSStation()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSStation <- function(){
  NRSStation <- readr::read_csv(paste0(pr_get_site(), "BGC_StationInfo.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    filter(.data$ProjectName == "NRS")
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @param Type A character string on which to filter data (P = Phytoplankton, Z = Zooplankton, F = Fish)
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSTrips <- function(Type = c("P","Z","F")){

  NRSTrip <- readr::read_csv(paste0(pr_get_site(), "BGC_Trip.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    rename(ZSampleDepth_m = .data$ZOOPSAMPLEDEPTH_M,
           PSampleDepth_m = .data$PHYTOSAMPLEDEPTH_M) %>%
    filter(.data$ProjectName == "NRS" &
             (stringr::str_detect(.data$SampleType, paste(Type, collapse = "|")) |
                is.na(.data$SampleType))) %>%
    pr_apply_time() %>%
    select(.data$TripCode:.data$SampleDateLocal, .data$Year:.data$SampleDateUTC, .data$Biomass_mgm3, .data$Secchi_m, .data$SampleType) %>%
    select(-.data$tz)


  if("P" %in% Type & !"Z" %in% Type){ # Only Phytoplankton
    NRSTrip <- NRSTrip %>%
      rename(SampleDepth_m = .data$PSampleDepth_m) %>%
      select(-.data$ZSampleDepth_m)
  }

  if("Z" %in% Type & !"P" %in% Type){ # Only Zooplankton
    NRSTrip <- NRSTrip %>%
      rename(SampleDepth_m = .data$ZSampleDepth_m) %>%
      select(-.data$PSampleDepth_m)
  }

  return(NRSTrip)

  # P - select(-c(.data$Biomass_mgm3, .data$Secchi_m)) %>%
  # Z - select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  # if("B" %in% Type){ # Return Biomass if its requested. Otherwise not.
  #   CPRSamps <- CPRSamps %>%
  #     select(-c(.data$PCI, .data$SampleType))
  # } else{
  #   CPRSamps <- CPRSamps %>%
  #     select(-c(.data$PCI, .data$SampleType, .data$Biomass_mgm3))
  # }



}

#
#' Get NRS Phytoplankton raw data - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRaw()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoRaw <- function(){

  NRSRawP <- left_join(pr_get_NRSTrips("P") %>%
                         select(-c(.data$Biomass_mgm3, .data$Secchi_m)),
                       pr_get_NRSPhytoData(), by = "TripCode") %>%
    select(-c(.data$TaxonGroup, .data$Genus, .data$Species, .data$Biovolume_um3L, .data$SPCode, .data$SampleType)) %>%
    arrange(-desc(.data$TaxonName)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal)) %>%
    select(-"NA")

}


#### Higher Trophic Groups Abund ####
#' Get Abundance of Phyto Higher Trophic Groups
#'
#' @return A dataframe with Phytoplankton Abundance data - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTG()
#' #' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoHTG <- function(){

  NRSHTGP <- pr_get_NRSPhytoData()%>%
    group_by(.data$TripCode, .data$TaxonGroup) %>%
    summarise(Cells_L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop") %>%
    filter(!.data$TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGP <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m)) %>%
    left_join(NRSHTGP, by = "TripCode") %>%
    mutate(TaxonGroup = if_else(is.na(.data$TaxonGroup), "Ciliate", .data$TaxonGroup),
           Cells_L = if_else(is.na(.data$Cells_L), 0, .data$Cells_L)) %>%
    arrange(-desc(.data$TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    select(-c(.data$SampleType))

}



#' Import NRS Phytoplankton Data
#'
#' Load NRS station Phytoplankton Data
#' @return A dataframe with NRS Phytoplankton Data in long form
#' @export
#' @examples
#' df <- pr_get_NRSPhytoData()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoData <- function(){
  NRSPdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



#' Import NRS Phytoplankton Changelog
#'
#' Load NRS Phytoplankton Changelog
#' @return A dataframe with NRS Phytoplankton Changelog
#' @export
#' @examples
#' df <- pr_get_NRSPhytoChangeLog()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoChangeLog <- function(){
  NRSPcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Phyto_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



#
#' Get NRS Phytoplankton genus product - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenus()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoGenus <- function() {

  # Bring in data once
  NRSSamp <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1)) %>%
    mutate(same = if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    filter(.data$same == "no")# no changes at genera level

  # for non change log species
  NRSGenP1 <- NRSPdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrslg$TaxonName)) & .data$Genus != '') %>%
    group_by(.data$TripCode, .data$Genus) %>%
    summarise(Cells_L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(.data$Genus)

  NRSGenP1 <- NRSSamp %>%
    left_join(NRSGenP1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = if_else(is.na(.data$Genus), "Acanthoica", .data$Genus),
           Cells_L = if_else(is.na(.data$Cells_L), 0, .data$Cells_L)) %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(Cells_L = sum(.data$Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA's and real absences as 0's
  NRSGenP2 <- NRSPdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrslg$TaxonName)) & .data$Genus != '') %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(.data$Genus)) %>%
    tidyr::drop_na(.data$Genus) %>%
    group_by(.data$TripCode, .data$StartDate, .data$Genus) %>%
    summarise(Cells_L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen = NRSGenP2 %>%
      select(.data$Genus) %>% unique()
    Gen = as.character(Gen$Genus[i] %>% droplevels())
    Dates <- as.data.frame(NRSGenP2) %>%
      filter(.data$Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      filter(.data$Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "P")) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Dates$Genus),
             Cells_L = replace(.data$Cells_L, .data$StartDate > .data$SampleDateLocal, -999),
             Cells_L = replace(.data$Cells_L, .data$StartDate < .data$SampleDateLocal & is.na(.data$Cells_L), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      summarise(Cells_L = sum(.data$Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSGenP1 <- bind_rows(NRSGenP1, gen)
  }

  NRSGenP1 <- NRSGenP1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(Cells_L = max(.data$Cells_L), .groups = "drop") %>%
    arrange(-desc(.data$Genus)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenP <-  NRSGenP1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(.data$SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}

#' Get NRS Phytoplankton species product - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpecies()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoSpecies <- function(){

  # Bring in data once
  NRSSamp <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    mutate(same = if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    filter(.data$same == "no") # no changes at genera level

  # for non change log species

  NRSSpecP1 <- NRSPdat %>%
    mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or cilliates etc.
    filter(!.data$TaxonName %in% levels(as.factor(nrsls$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("/", .data$Species)) %>%
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(Cells_L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop")

  NRSSpecP1 <- NRSSamp %>%
    left_join(NRSSpecP1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = if_else(is.na(.data$TaxonName), "Paralia sulcata", .data$TaxonName),
           Cells_L = if_else(is.na(.data$Cells_L), 0, .data$Cells_L)) %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    summarise(Cells_L = sum(.data$Cells_L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSSpecP2 <- NRSPdat %>%
    mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or cilliates etc.
    filter(.data$TaxonName %in% levels(as.factor(nrsls$TaxonName)) & .data$TaxonName != '' &
             .data$Species != "spp." & .data$Species != "" & !is.na(.data$Species) & !grepl("cf.", .data$Species)) %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(TaxonName = forcats::as_factor(.data$TaxonName)) %>%
    tidyr::drop_na(.data$TaxonName) %>%
    group_by(.data$TripCode, .data$StartDate, .data$TaxonName) %>%
    summarise(Cells_L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecP2$TaxonName)) {
    Taxon = NRSSpecP2 %>%
      select(.data$TaxonName) %>%
      unique()

    Taxon = as.character(Taxon$TaxonName[i] %>% droplevels())

    Dates <- as.data.frame(NRSSpecP2) %>%
      filter(.data$TaxonName == Taxon) %>%
      slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecP2) %>%
      filter(.data$TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "P")) %>%
      left_join(spec, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             TaxonName = replace(.data$TaxonName, is.na(.data$TaxonName), Dates$TaxonName),
             Cells_L = replace(.data$Cells_L, .data$StartDate>.data$SampleDateLocal, -999),
             Cells_L = replace(.data$Cells_L, .data$StartDate<.data$SampleDateLocal & is.na(.data$Cells_L), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
      summarise(Cells_L = sum(.data$Cells_L), .groups = "drop") %>%
      as.data.frame()
    NRSSpecP1 <- bind_rows(NRSSpecP1, spec)
  }

  NRSSpecP1 <- NRSSpecP1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    summarise(Cells_L = max(.data$Cells_L), .groups = "drop") %>%
    arrange(-desc(.data$TaxonName)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecP <-  NRSSpecP1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$Cells_L, values_fill = list(Cells_L = 0)) %>%
    arrange(desc(.data$SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}


#' Get NRS Phytoplankton raw product - Biovolume
#'
#' @return A dataframe with raw NRS Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRawBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoRawBV <- function(){
  NRSRawP <- left_join(pr_get_NRSTrips("P") %>%
                         select(-c(.data$Biomass_mgm3, .data$Secchi_m)),
                       pr_get_NRSPhytoData(), by = "TripCode") %>%
    select(-c(.data$TaxonGroup, .data$Genus, .data$Species, .data$Cells_L, .data$SPCode, .data$SampleType)) %>%
    arrange(-desc(.data$TaxonName)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$Biovolume_um3L, values_fill = list(Biovolume_um3L = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}

#' Get NRS Phytoplankton higher taxon group product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTGBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoHTGBV <- function() {
  NRSHTGPB1 <- pr_get_NRSPhytoData() %>%
    group_by(.data$TripCode, .data$TaxonGroup) %>%
    summarise(BioV_um3L = sum(.data$Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    filter(!.data$TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  NRSHTGPB1 <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m)) %>%
    left_join(NRSHTGPB1, by = "TripCode") %>%
    mutate(TaxonGroup = if_else(is.na(.data$TaxonGroup), "Ciliate", .data$TaxonGroup),
           BioV_um3L = if_else(is.na(.data$BioV_um3L), 0, .data$BioV_um3L)) %>%
    arrange(-desc(.data$TaxonGroup))

  NRSHTGPB <-  NRSHTGPB1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    select(-c(.data$TripCode, .data$SampleType))
}

#' Get NRS Phytoplankton genus product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenusBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoGenusBV <- function(){

  # Bring in all NRS phytoplankton samples, data and changelog once
  NRSSamp <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check genus are effected by change log
  nrslg <- NRSPcl %>%
    mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1)) %>%
    mutate(same = if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    filter(.data$same == "no")# no changes at genera level

  # for non change log species
  NRSGenPB1 <- NRSPdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrslg$TaxonName)) & .data$Genus != "") %>%
    group_by(.data$TripCode, .data$Genus) %>%
    summarise(BioV_um3L = sum(.data$Biovolume_um3L, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(.data$Genus)

  NRSGenPB1 <- NRSSamp %>%
    left_join(NRSGenPB1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = if_else(is.na(.data$Genus), "Acanthoica", .data$Genus),
           BioV_um3L = if_else(is.na(.data$BioV_um3L), 0, .data$BioV_um3L)) %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(BioV_um3L = sum(.data$BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenPB2 <- NRSPdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrslg$TaxonName)) & .data$Genus != "") %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(.data$Genus)) %>%
    tidyr::drop_na(.data$Genus) %>%
    group_by(.data$TripCode, .data$StartDate, .data$Genus) %>%
    summarise(BioV_um3L = sum(.data$Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenPB2$Genus)) {
    Gen = NRSGenPB2 %>%
      select(.data$Genus) %>%
      unique()

    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenPB2) %>%
      filter(.data$Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenPB2) %>%
      filter(.data$Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "P")) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Dates$Genus),
             BioV_um3L = replace(.data$BioV_um3L, .data$StartDate>.data$SampleDateLocal, -999),
             BioV_um3L = replace(.data$BioV_um3L, .data$StartDate<.data$SampleDateLocal & is.na(.data$BioV_um3L), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      summarise(BioV_um3L = sum(.data$BioV_um3L), .groups = "drop") %>%
      as.data.frame()

    NRSGenPB1 <- bind_rows(NRSGenPB1, gen)
  }

  NRSGenPB1 <- NRSGenPB1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(BioV_um3L = max(.data$BioV_um3L), .groups = "drop") %>%
    arrange(-desc(.data$Genus)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified
  NRSGenPB <-  NRSGenPB1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(.data$SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}

#' Get NRS Phytoplankton species product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpeciesBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoSpeciesBV <- function(){

  # Bring in needed data once
  NRSSamp <- pr_get_NRSTrips("P") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  NRSPdat <- pr_get_NRSPhytoData()

  NRSPcl <- pr_get_NRSPhytoChangeLog()

  # Check at what level we need change log
  nrsls <- NRSPcl %>%
    mutate(same = if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    filter(.data$same == "no") # no changes at genera level

  # for non change log species

  NRSPdat1 <- NRSPdat %>%
    mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    filter(!grepl("^(?!.*/~\\$)", .data$TaxonName, perl = TRUE) &
             .data$Species == "spp." &
             !.data$TaxonName %in% levels(as.factor(nrsls$TaxonName)) )

  NRSSpecPB1 <- NRSPdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrsls$TaxonName)) &
             .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species)) %>%
    bind_rows(NRSPdat1) %>%
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(BioV_um3L = sum(.data$Biovolume_um3L, na.rm = TRUE), .groups = "drop")

  NRSSpecPB1 <- NRSSamp %>%
    left_join(NRSSpecPB1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = if_else(is.na(.data$TaxonName), "Paralia sulcata", .data$TaxonName),
           BioV_um3L = if_else(is.na(.data$BioV_um3L), 0, .data$BioV_um3L)) %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    summarise(BioV_um3L = sum(.data$BioV_um3L), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSPdat2 <- NRSPdat %>%
    mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    filter(!grepl("^(?!.*/~\\$)", .data$TaxonName, perl = TRUE) &
             .data$Species == "spp." &
             .data$TaxonName %in% levels(as.factor(nrsls$TaxonName)))

  NRSSpecPB2 <- NRSPdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrsls$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & .data$Species != "") %>%
    bind_rows(NRSPdat2) %>%
    left_join(NRSPcl, by = "TaxonName") %>%
    filter(.data$TaxonName != '') %>%
    mutate(TaxonName = forcats::as_factor(.data$TaxonName)) %>%
    tidyr::drop_na(.data$TaxonName) %>%
    group_by(.data$TripCode, .data$StartDate, .data$TaxonName) %>%
    summarise(BioV_um3L = sum(.data$Cells_L, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSSpecPB2$TaxonName)) {
    Taxon = NRSSpecPB2 %>%
      select(.data$TaxonName) %>%
      unique()

    Taxon = as.character(Taxon$TaxonName[i] %>%
                           droplevels())

    Dates <- as.data.frame(NRSSpecPB2) %>%
      filter(.data$TaxonName == Taxon) %>%
      slice(1)  %>%
      droplevels()

    spec <- as.data.frame(NRSSpecPB2) %>%
      filter(.data$TaxonName == Taxon) %>%
      droplevels()

    spec <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "P")) %>%
      left_join(spec, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             TaxonName = replace(.data$TaxonName, is.na(.data$TaxonName), Dates$TaxonName),
             BioV_um3L = replace(.data$BioV_um3L, .data$StartDate>.data$SampleDateLocal, -999),
             BioV_um3L = replace(.data$BioV_um3L, .data$StartDate<.data$SampleDateLocal & is.na(.data$BioV_um3L), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
      summarise(BioV_um3L = sum(.data$BioV_um3L), .groups = "drop") %>%
      as.data.frame()

    NRSSpecPB1 <- bind_rows(NRSSpecPB1, spec)
  }

  NRSSpecPB1 <- NRSSpecPB1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    summarise(BioV_um3L = max(.data$BioV_um3L), .groups = "drop") %>%
    arrange(-desc(.data$TaxonName)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSSpecPB <-  NRSSpecPB1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$BioV_um3L, values_fill = list(BioV_um3L = 0)) %>%
    arrange(desc(.data$SampleDateLocal))  %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}


#' Load zooplankton abundance data
#'
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooData()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooData <- function(){
  NRSZdat <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



#' Load zooplankton NRS Zooplankton changelog
#'
#' @return A dataframe with NRS zooplankton changelog
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooChangeLog()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooChangeLog <- function(){
  NRSZcl <- readr::read_csv(paste0(pr_get_site(), "BGC_Zoop_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get NRS Zoop raw product - Abundance
#'
#' @return A dataframe with Raw NRS Zooplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooRaw()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooRaw <- function(){

  NRSRawZ <- left_join(pr_get_NRSTrips("Z") %>%
                         select(-c(.data$Biomass_mgm3, .data$Secchi_m)),
                       pr_get_NRSZooData(), by = "TripCode") %>%
    select(-c(.data$Copepod, .data$TaxonGroup, .data$Genus, .data$Species, .data$SampleType, .data$SPCode)) %>%
    arrange(-desc(.data$TaxonName)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal)) %>%
    select(-"NA")

}

#' NRS Zoop raw product binned by sex and stage raw product
#'
#' @return A dataframe with NRS Zooplankton Abundance - Binned by sex and stage
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooRawBin()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooRawBin <- function(){
  NRSIdsZ <- left_join(pr_get_NRSTrips("Z") %>%
                         select(-c(.data$Biomass_mgm3, .data$Secchi_m)),
                       pr_get_NRSZooData(), by = "TripCode") %>%
    mutate(TaxonName = if_else(is.na(.data$Genus), .data$TaxonName, paste0(.data$Genus, ' ', .data$Species))) %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$SampleDateUTC, .data$TaxonName) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE)) %>%
    arrange(-desc(.data$TaxonName))  %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}


#' Get NRS Zoop HTG product - Abundance
#'
#' @return A dataframe with NRS Zooplankton Abundance - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooHTG()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooHTG <-  function(){
  nrsHTGZ1 <- pr_get_NRSZooData() %>%
    group_by(.data$TripCode, .data$TaxonGroup) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    filter(!.data$TaxonGroup %in% c("Other"))

  nrsHTGZ1 <-  pr_get_NRSTrips("Z") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m)) %>%
    left_join(nrsHTGZ1, by = "TripCode") %>%
    mutate(TaxonGroup = if_else(is.na(.data$TaxonGroup), "Copepod", .data$TaxonGroup),
           ZoopAbund_m3 = if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%
    arrange(-desc(.data$TaxonGroup))

  nrsHTGZ <-  nrsHTGZ1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    select(-c(.data$SampleType)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}

#' NRS Zoop genus product - Abundance
#'
#' @return A dataframe with NRS Zooplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooGenus()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips("Z") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  # Check genus are effected by change log
  nrszlg <- NRSZcl %>%
    mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1)) %>%
    mutate(same = if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    filter(.data$same == "no")# no changes at genera level

  # for non change log species
  NRSGenZ1 <- NRSZdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & .data$Genus != "") %>%
    group_by(.data$TripCode, .data$Genus) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(.data$Genus)

  NRSGenZ1 <- NRSSamp %>%
    left_join(NRSGenZ1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = if_else(is.na(.data$Genus), "Acanthoica", stringr::word(.data$Genus,1)), # bin subgenera together
           ZoopAbund_m3 = if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3))  %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSGenP2 <- NRSZdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrszlg$TaxonName)) & .data$Genus != "") %>%
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Genus = forcats::as_factor(.data$Genus)) %>%
    tidyr::drop_na(.data$Genus) %>%
    group_by(.data$TripCode, .data$StartDate, .data$Genus) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSGenP2$Genus)) {
    Gen <- NRSGenP2 %>%
      select(.data$Genus) %>%
      unique()

    Gen = as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(NRSGenP2) %>%
      filter(.data$Genus == Gen) %>%
      slice(1) %>%
      droplevels()

    gen <- as.data.frame(NRSGenP2) %>%
      filter(.data$Genus == Gen) %>%
      droplevels()

    gen <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "Z")) %>%
      left_join(gen, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Dates$Genus),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateLocal, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateLocal & is.na(.data$ZoopAbund_m3), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSGenZ1 <- bind_rows(NRSGenZ1, gen)
  }

  NRSGenZ1 <- NRSGenZ1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    arrange(-desc(.data$Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSGenZ <- NRSGenZ1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}

#' NRS Zoop copepod product - Abundance
#'
#' @return A dataframe with NRS Copepod Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooSpeciesCopepod()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooSpeciesCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips("Z") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    mutate(same = if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    filter(.data$same == "no") # no changes at genera level

  # for non change log species

  NRSCop1 <- NRSZdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrsclc$TaxonName)) &
             .data$Copepod =="COPEPOD" &
             .data$Species != "spp." &
             !is.na(.data$Species) &
             !grepl("cf.", .data$Species) &
             !grepl("/", .data$Species) &
             !grepl("grp", .data$Species)
           ) %>%
    mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSCop1 <- NRSSamp %>%
    left_join(NRSCop1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = if_else(is.na(.data$Species), "Calanus Australis", .data$Species), # avoids nulls in pivot
           ZoopAbund_m3 = if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%  # avoids nulls in pivot
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSCop2 <- NRSZdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & .data$Copepod =="COPEPOD"
           & .data$Species != "spp." & !is.na(.data$Species)  & .data$Species != "" &
             !grepl("/", .data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Species = forcats::as_factor(.data$Species)) %>%
    tidyr::drop_na(.data$Species) %>%
    group_by(.data$TripCode, .data$StartDate, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSCop2$Species)) {

    Taxon <- NRSCop2 %>%
      select(.data$Species) %>%
      unique()

    Taxon <- as.character(Taxon$Species[i] %>%
                            droplevels())

    Dates <- as.data.frame(NRSCop2) %>%
      filter(.data$Species == Taxon) %>%
      slice(1) %>%
      droplevels()

    copes <- as.data.frame(NRSCop2) %>%
      filter(.data$Species == Taxon) %>%
      droplevels()

    copes <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "Z")) %>%
      left_join(copes, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Species = replace(.data$Species, is.na(.data$Species), Dates$Species),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateLocal, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateLocal & is.na(.data$ZoopAbund_m3), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
      summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()
    NRSCop1 <- bind_rows(NRSCop1, copes)
  }

  NRSCop1 <- NRSCop1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    arrange(-desc(.data$Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  NRSCop <-  NRSCop1 %>%
    tidyr::pivot_wider(names_from = .data$Species, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}


#' Get NRS Zoop Non-Copepod Abundance Data
#'
#' @return A dataframe with NRS Zooplankton (non-copepod) Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooSpeciesNonCopepod()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooSpeciesNonCopepod <- function(){

  # Bring in all NRS zooplankton samples, data and changelog once
  NRSZdat <- pr_get_NRSZooData()

  NRSZcl <- pr_get_NRSZooChangeLog()

  NRSSamp <- pr_get_NRSTrips("Z") %>%
    select(-c(.data$Biomass_mgm3, .data$Secchi_m))

  # Check at what level we need change log
  nrsclc <- NRSZcl %>%
    mutate(same = if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    filter(.data$same == "no") # no changes at genera level

  # for non change log species
  NRSnCop1 <- NRSZdat %>%
    filter(!.data$TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & .data$Copepod =="NON-COPEPOD"
          ) %>%
    pr_filter_species() %>%
    mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  NRSnCop1 <- NRSSamp %>%
    left_join(NRSnCop1, by = "TripCode") %>%
    mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = if_else(is.na(.data$Species), "Calanus Australis", .data$Species), # avoids nulls in pivot
           ZoopAbund_m3 = if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%  # avoids nulls in pivot
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  NRSnCop2 <- NRSZdat %>%
    filter(.data$TaxonName %in% levels(as.factor(nrsclc$TaxonName)) & .data$Copepod =="NON-COPEPOD"  & .data$Species != ""
          ) %>%
    pr_filter_species() %>%
    mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    left_join(NRSZcl, by = "TaxonName") %>%
    mutate(Species = forcats::as_factor(.data$Species)) %>%
    tidyr::drop_na(.data$Species) %>%
    group_by(.data$TripCode, .data$StartDate, .data$Species) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(NRSnCop2$Species)) {
    Taxon <- NRSnCop2 %>%
      select(.data$Species) %>%
      unique()

    Taxon <- as.character(Taxon$Species[i] %>% droplevels())

    Dates <- as.data.frame(NRSnCop2) %>%
      filter(.data$Species == Taxon) %>%
      slice(1) %>%
      droplevels()

    ncopes <- as.data.frame(NRSnCop2) %>%
      filter(.data$Species == Taxon) %>%
      droplevels()

    ncopes <- NRSSamp %>%
      filter(stringr::str_detect(.data$SampleType, "Z")) %>%
      left_join(ncopes, by = "TripCode") %>%
      mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Species = replace(.data$Species, is.na(.data$Species), Dates$Species),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateLocal, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateLocal & is.na(.data$ZoopAbund_m3), 0)) %>%
      group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
               .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
      summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()

    NRSnCop1 <- bind_rows(NRSnCop1, ncopes)
  }

  NRSnCop1 <- NRSnCop1 %>%
    group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
             .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    arrange(-desc(.data$Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  NRSnCop <- NRSnCop1 %>%
    tidyr::pivot_wider(names_from = .data$Species, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    arrange(desc(.data$SampleDateLocal)) %>%
    mutate(SampleDateLocal = as.character(.data$SampleDateLocal))
}



#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPigments()
pr_get_NRSPigments <- function(){
  Pigments <- readr::read_csv(paste0(pr_get_site(),"BGC_Pigments.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPico <- function(){
  Pico <- readr::read_csv(paste0(pr_get_site(), "BGC_Picoplankton.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

