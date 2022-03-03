#' Get CPR trips
#'
#' @return A dataframe with CPR Trips
#' @export
#'
#' @examples
#' df <- pr_get_CPRTrips()
#' @importFrom rlang .data
pr_get_CPRTrips <- function(){
  CPRTrips <- readr::read_csv(paste0(pr_get_site2(), "CPR_Trip.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get CPR samples
#'
#' @param Type A character string on which to filter data (P = Phytoplankton, Z = Zooplankton, B = Biomass)
#'
#' @return A dataframe with CPR Samples
#' @export
#'
#' @examples
#' df <- pr_get_CPRSamps("Z")
#' @importFrom rlang .data
pr_get_CPRSamps <- function(Type = c("P","Z","B")){

  CPRSamps <- readr::read_csv(paste0(pr_get_site2(), "CPR_Samp.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::filter(stringr::str_detect(.data$SampleType, paste(Type, collapse = "|"))) %>%
    dplyr::mutate(Year = lubridate::year(.data$SampleDateUTC),
           Month = lubridate::month(.data$SampleDateUTC),
           Day = lubridate::day(.data$SampleDateUTC),
           Time_24hr = stringr::str_sub(.data$SampleDateUTC, -8, -1)) %>%
    pr_add_bioregions() %>%
    dplyr::select(c(.data$TripCode, .data$Sample, .data$Latitude:.data$SampleDateLocal, .data$Year:.data$BioRegion, .data$PCI, .data$Biomass_mgm3, .data$SampleType))

  if("B" %in% Type){ # Return Biomass if its requested. Otherwise not.
    CPRSamps <- CPRSamps %>%
      dplyr::select(-c(.data$PCI, .data$SampleType))
  } else{
    CPRSamps <- CPRSamps %>%
      dplyr::select(-c(.data$PCI, .data$SampleType, .data$Biomass_mgm3))
  }
}

#' Get CPR Phytoplankton Abundance or Count data
#'
#' @param var A string of either "Abundance" or "Count"
#' @return A dataframe with CPR Phytoplankton Abundance or Count data
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoData()
#' @importFrom rlang .data
pr_get_CPRPhytoData <- function(var = "Abundance"){
  cprPdat <- readr::read_csv(paste0(pr_get_site2(), "CPR_Phyto_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()

  if(stringr::str_detect(var, "Abundance")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$FovCount, .data$SampVol_m3, .data$BioVolume_um3m3))
  } else if(stringr::str_detect(var, "Count")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$BioVolume_um3m3, .data$PhytoAbund_m3))
  } else if(stringr::str_detect(var, "Biovolume")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$FovCount, .data$SampVol_m3, .data$PhytoAbund_m3))
  } else if(stringr::str_detect(var, "All")){
    cprPdat <- cprPdat # Do nothing. Return all variables
  }

}


#' Get Phyto Change Log
#'
#' @return A dataframe with the CPR Phytoplankton Changelog
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoChangeLog()
#' @importFrom rlang .data
pr_get_CPRPhytoChangeLog <- function(){
  cprPcl <- readr::read_csv(paste0(pr_get_site2(), "CPR_Phyto_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

#' Get CPR Zooplankton abundance data
#'
#' @param var A string of either "Abundance" or "Count"
#' @return A dataframe with CPR Zooplankton Abundance data
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooData()
#' @importFrom rlang .data
pr_get_CPRZooData <- function(var = "Abundance"){

  cprZdat <- readr::read_csv(paste0(pr_get_site2(), "CPR_Zoop_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()

  if(stringr::str_detect(var, "Abundance")){
    cprZdat <- cprZdat %>%
      dplyr::select(-c(.data$TaxonCount, .data$SampVol_m3))
  } else if(stringr::str_detect(var, "Count")){
    cprZdat <- cprZdat %>%
      dplyr::select(-.data$ZoopAbund_m3)
  }
}

#' Get CPR zooplankton Change Log
#'
#' @return A dataframe with the CPR Zooplankton Changelog
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooChangeLog()
#' @importFrom rlang .data
pr_get_CPRZooChangeLog <- function(){
  cprZcl <- readr::read_csv(paste0(pr_get_site2(), "CPR_Zoop_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

################################################################################################################################################
#' Get CPR Phyto raw product - Abundance
#'
#' @return A dataframe with CPR Raw Phytoplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRaw()
#' @importFrom rlang .data
pr_get_CPRPhytoRaw <- function(){

  cprRawP <- pr_get_CPRSamps("P") %>%
    dplyr::left_join(pr_get_CPRPhytoData("Abundance"), by = "Sample") %>%
    dplyr::select(c(.data$Sample:.data$TaxonName,.data$PhytoAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName)) %>%
    dplyr::mutate(TaxonName = dplyr::if_else(is.na(.data$TaxonName), "No taxa found", .data$TaxonName)) %>% # for segments where no phyto was found
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-"No taxa found")

}

#' CPR Phyto HTG product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Higher Trophic Groups
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoHTG()
#' @importFrom rlang .data
pr_get_CPRPhytoHTG <- function(){

  cprHTGP1 <- pr_get_CPRPhytoData("Abundance") %>%
    dplyr::group_by(.data$Sample, .data$TaxonGroup) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!.data$TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGP <- pr_get_CPRSamps("P") %>%
    dplyr::left_join(cprHTGP1, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = dplyr::if_else(is.na(.data$TaxonGroup), "Ciliate", .data$TaxonGroup),
           PhytoAbund_m3 = dplyr::if_else(is.na(.data$PhytoAbund_m3), 0, .data$PhytoAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-.data$Sample)
}

#' Get CPR Phyto genus product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoGenus()
#' @importFrom rlang .data
pr_get_CPRPhytoGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData("Abundance")

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1),
           same = dplyr::if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    dplyr::filter(.data$same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps("P")

  # for non change log species

  cprGenP1 <- cprPdat %>%
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::group_by(.data$Sample, .data$Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!.data$Genus == "")

  cprGenP1 <- cprSamp %>%
    dplyr::left_join(cprGenP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = dplyr::if_else(is.na(.data$Genus), "Acanthoica", .data$Genus),
           PhytoAbund_m3 = dplyr::if_else(is.na(.data$PhytoAbund_m3), 0, .data$PhytoAbund_m3)) %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenP2 <- cprPdat  %>%
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(.data$Genus != '') %>%
    dplyr::mutate(Genus = forcats::as_factor(.data$Genus)) %>%
    tidyr::drop_na(.data$Genus) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenP2$Genus)) {
    Gen <- cprGenP2 %>%
      dplyr::select(.data$Genus) %>%
      unique()

    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(cprGenP2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      dplyr::slice(1)  %>%
      droplevels()

    gen <- as.data.frame(cprGenP2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      droplevels()

    gen <- cprSamp %>%
      dplyr::left_join(gen, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Dates$Genus),
             PhytoAbund_m3 = replace(.data$PhytoAbund_m3, .data$StartDate>.data$SampleDateUTC, -999),
             PhytoAbund_m3 = replace(.data$PhytoAbund_m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$PhytoAbund_m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()

    cprGenP1 <- dplyr::bind_rows(cprGenP1, gen)
  }

  cprGenP1 <- cprGenP1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = max(.data$PhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenP <-  cprGenP1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#' Get CPR Phyto species product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoSpecies()
#' @importFrom rlang .data
pr_get_CPRPhytoSpecies <-  function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData("Abundance")

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(same = dplyr::if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    dplyr::filter(.data$same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps("P")

  # for non change log species
  cprSpecP1 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species)) %>%
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  cprSpecP1 <- cprSamp %>%
    dplyr::left_join(cprSpecP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = dplyr::if_else(is.na(.data$TaxonName), "Paralia sulcata", .data$TaxonName),
           PhytoAbund_m3 = dplyr::if_else(is.na(.data$PhytoAbund_m3), 0, .data$PhytoAbund_m3))  %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprSpecP2 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species)
           & !grepl("cf.", .data$Species)) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(.data$TaxonName != '') %>%
    dplyr::mutate(TaxonName = forcats::as_factor(.data$TaxonName)) %>%
    tidyr::drop_na(.data$TaxonName) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprSpecP2$TaxonName)) {
    Spe <- cprSpecP2 %>%
      dplyr::select(.data$TaxonName) %>%
      unique()

    Spe <- as.character(Spe$TaxonName[i] %>%
                          droplevels())

    Dates <- as.data.frame(cprSpecP2) %>%
      dplyr::filter(.data$TaxonName == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    spec <- as.data.frame(cprSpecP2) %>%
      dplyr::filter(.data$TaxonName == Spe) %>%
      droplevels()

    spec <- cprSamp %>%
      dplyr::left_join(spec, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             TaxonName = replace(.data$TaxonName, is.na(.data$TaxonName), Dates$TaxonName),
             PhytoAbund_m3 = replace(.data$PhytoAbund_m3, .data$StartDate>.data$SampleDateUTC, -999),
             PhytoAbund_m3 = replace(.data$PhytoAbund_m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$PhytoAbund_m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
      dplyr::summarise(PhytoAbund_m3 = sum(.data$PhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprSpecP1 <- dplyr::bind_rows(cprSpecP1, spec)
  }

  cprSpecP1 <- cprSpecP1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = max(.data$PhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified
  cprSpecP <-  cprSpecP1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

###############################################################################################################################################
#' Get CPR Phyto raw product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRawBV()
#' @importFrom rlang .data
pr_get_CPRPhytoRawBV <- function(){
  cprRawP <- pr_get_CPRSamps("P") %>%
    dplyr::left_join(pr_get_CPRPhytoData("Biovolume"), by = "Sample") %>%
    dplyr::select(c(.data$Sample:.data$TaxonName,.data$BioVolume_um3m3)) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName)) %>%
    dplyr::mutate(TaxonName = dplyr::if_else(is.na(.data$TaxonName), "No taxa found", .data$TaxonName)) %>% # for segments where no phyto was found
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$BioVolume_um3m3, values_fill = list(BioVolume_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-"No taxa found")
}

#' Get CPR Phyto HTG product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoHTGBV()
#' @importFrom rlang .data
pr_get_CPRPhytoHTGBV <- function(){
  cprHTGPB1 <- pr_get_CPRPhytoData("Biovolume") %>%
    dplyr::group_by(.data$Sample, .data$TaxonGroup) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$BioVolume_um3m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!.data$TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGPB1 <-  pr_get_CPRSamps("P")  %>%
    dplyr::left_join(cprHTGPB1, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = dplyr::if_else(is.na(.data$TaxonGroup), "Ciliate", .data$TaxonGroup),
           PBioV_um3m3 = dplyr::if_else(is.na(.data$PBioV_um3m3), 0, .data$PBioV_um3m3)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-.data$Sample)
}

#' Get CPR Phyto genus product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoGenusBV()
#' @importFrom rlang .data
pr_get_CPRPhytoGenusBV <- function(){

  cprPdat <- pr_get_CPRPhytoData("Biovolume")

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1)) %>%
    dplyr::mutate(same = dplyr::if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    dplyr::filter(.data$same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps("P")

  # for non change log species

  cprGenPB1 <- cprPdat %>%
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName)) & .data$Genus != '') %>%
    dplyr::group_by(.data$Sample, .data$Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$BioVolume_um3m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(.data$Genus)

  cprGenPB1 <- cprSamp %>%
    dplyr::left_join(cprGenPB1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = dplyr::if_else(is.na(.data$Genus), "Acanthoica", .data$Genus),
           PBioV_um3m3 = dplyr::if_else(is.na(.data$PBioV_um3m3), 0, .data$PBioV_um3m3)) %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$PBioV_um3m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenPB2 <- cprPdat %>%
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(.data$Genus != '') %>%
    dplyr::mutate(Genus = forcats::as_factor(.data$Genus)) %>% tidyr::drop_na(.data$Genus) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$Genus) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenPB2$Genus)) {
    Gen <- cprGenPB2 %>%
      dplyr::select(.data$Genus) %>% unique()
    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Dates <- as.data.frame(cprGenPB2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      dplyr::slice(1)  %>%
      droplevels()

    gen <- as.data.frame(cprGenPB2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      droplevels()

    gen <- cprSamp %>%
      dplyr::left_join(gen, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Dates$Genus),
             PBioV_um3m3 = replace(.data$PBioV_um3m3, .data$StartDate>.data$SampleDateUTC, -999),
             PBioV_um3m3 = replace(.data$PBioV_um3m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$PBioV_um3m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      dplyr::summarise(PBioV_um3m3 = sum(.data$PBioV_um3m3), .groups = "drop") %>%
      as.data.frame()

    cprGenPB1 <- dplyr::bind_rows(cprGenPB1, gen)
  }

  cprGenPB1 <- cprGenPB1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(PBioV_um3m3 = max(.data$PBioV_um3m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified

  cprGenPB <-  cprGenPB1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#' Get CPR Phyto species product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoSpeciesBV()
#' @importFrom rlang .data
pr_get_CPRPhytoSpeciesBV <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData("Biovolume")

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(same = dplyr::if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    dplyr::filter(.data$same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps("P")

  # for non change log species
  cprSpecPB1 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species)) %>%
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  cprSpecPB1 <- cprSamp %>%
    dplyr::left_join(cprSpecPB1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           TaxonName = dplyr::if_else(is.na(.data$TaxonName), "Paralia sulcata", .data$TaxonName),
           PBioV_um3m3 = dplyr::if_else(is.na(.data$PBioV_um3m3), 0, .data$PBioV_um3m3))  %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$PBioV_um3m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprSpecPB2 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus, ' ', .data$Species)) %>% # remove .data$Comments about with flagellates or ciliates etc.
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprPcl$TaxonName))
           & .data$Species != "spp." & !is.na(.data$Species)
           & !grepl("cf.", .data$Species)) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(.data$TaxonName != '') %>%
    dplyr::mutate(TaxonName = forcats::as_factor(.data$TaxonName)) %>%
    tidyr::drop_na(.data$TaxonName) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = sum(.data$BioVolume_um3m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprSpecPB2$TaxonName)) {
    Spe <- cprSpecPB2 %>%
    dplyr::select(.data$TaxonName) %>%
    unique()

    Spe <- as.character(Spe$TaxonName[i] %>%
      droplevels())

    Dates <- as.data.frame(cprSpecPB2) %>%
      dplyr::filter(.data$TaxonName == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    spec <- as.data.frame(cprSpecPB2) %>%
      dplyr::filter(.data$TaxonName == Spe) %>%
      droplevels()

    spec <- cprSamp %>%
      dplyr::left_join(spec, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             TaxonName = replace(.data$TaxonName, is.na(.data$TaxonName), Dates$TaxonName),
             PBioV_um3m3 = replace(.data$PBioV_um3m3, .data$StartDate>.data$SampleDateUTC, -999),
             PBioV_um3m3 = replace(.data$PBioV_um3m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$PBioV_um3m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
      dplyr::summarise(PBioV_um3m3 = sum(.data$PBioV_um3m3), .groups = "drop") %>%
      as.data.frame()

    cprSpecPB1 <- dplyr::bind_rows(cprSpecPB1, spec)
  }

  cprSpecPB1 <- cprSpecPB1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    dplyr::summarise(PBioV_um3m3 = max(.data$PBioV_um3m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName)) %>%
    as.data.frame()

  # select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified
  cprSpecPB <-  cprSpecPB1 %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$PBioV_um3m3, values_fill = list(PBioV_um3m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#### CPR Zooplankton #### ################################################################################################################################
#' Get CPR Zoop raw product - Abundance
#'
#' @return A dataframe with Raw CPR Zooplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooRaw()
#' @importFrom rlang .data
pr_get_CPRZooRaw <- function(){
  cprRawZ <- pr_get_CPRSamps("Z") %>%
    dplyr::left_join(pr_get_CPRZooData("Abundance"), by = "Sample") %>%
    dplyr::select(-c("Copepod", "TaxonGroup", "Genus", "Species", "SPCode")) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName)) %>%
    dplyr::mutate(TaxonName = dplyr::if_else(is.na(.data$TaxonName), "No taxa found", .data$TaxonName)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-"No taxa found") %>%
    dplyr::select(-.data$Sample)
}

#' CPR Zoop raw product binned by sex and stage raw product
#'
#' @return A dataframe with CPR Zooplankton Abundance - Binned by sex and stage
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooRawSS()
#' @importFrom rlang .data
pr_get_CPRZooRawSS <- function(){
  CPRIdsZ <- pr_get_CPRSamps("Z") %>%
    dplyr::left_join(pr_get_CPRZooData("Abundance"), by = "Sample") %>%
    dplyr::mutate(TaxonName = dplyr::if_else(is.na(.data$Genus), .data$TaxonName, paste0(.data$Genus, ' ', .data$Species))) %>%
    dplyr::group_by(.data$TripCode, .data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$TaxonName) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE)) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonName))  %>%
    tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#' Get CPR Zoop HTG product - Abundance
#'
#' @return A dataframe with CPR Zooplankton Abundance - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooHTG()
#' @importFrom rlang .data
pr_get_CPRZooHTG <- function(){
  cprHTGZ <- pr_get_CPRZooData("Abundance") %>%
    dplyr::group_by(.data$Sample, .data$TaxonGroup) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!.data$TaxonGroup %in% c("Other"))

  cprHTGZ1 <- pr_get_CPRSamps("Z") %>%
    dplyr::left_join(cprHTGZ, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = dplyr::if_else(is.na(.data$TaxonGroup), "Copepod", .data$TaxonGroup),
           ZoopAbund_m3 = dplyr::if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(.data$TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = .data$TaxonGroup, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC)) %>%
    dplyr::select(-.data$Sample)
}

#' CPR Zoop genus product - Abundance
#'
#' @return A dataframe with CPR Zooplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooGenus()
#' @importFrom rlang .data
pr_get_CPRZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData("Abundance")

  cprZcl <- pr_get_CPRZooChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(.data$TaxonName, 1),
           genus2 = stringr::word(.data$ParentName, 1)) %>%
    dplyr::mutate(same = dplyr::if_else(.data$genus1==.data$genus2, "yes", "no")) %>%
    dplyr::filter(.data$same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps("Z")

  # for non change log species
  cprGenZ1 <- cprZdat %>%
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::group_by(.data$Sample, .data$Genus) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(.data$Genus != '')

  cprGenZ1 <- cprSamp %>%
    dplyr::left_join(cprGenZ1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Genus = dplyr::if_else(is.na(.data$Genus), "Calanus", .data$Genus),
           ZoopAbund_m3 = dplyr::if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenZ2 <- cprZdat %>%
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    dplyr::filter(.data$Genus != '')  %>%
    dplyr::mutate(Genus = forcats::as_factor(.data$Genus)) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$Genus) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprGenZ2$Genus)) {
    Gen <- cprGenZ2 %>%
      dplyr::select(.data$Genus) %>%
      unique()
    Gen <- as.character(Gen$Genus[i] %>% droplevels())

    Datesz <- as.data.frame(cprGenZ2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      dplyr::slice(1) %>%
      droplevels()

    genz <- as.data.frame(cprGenZ2) %>%
      dplyr::filter(.data$Genus == Gen) %>%
      droplevels()

    genz <- cprSamp %>%
      dplyr::left_join(genz, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Datesz$StartDate),
             Genus = replace(.data$Genus, is.na(.data$Genus), Datesz$Genus),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateUTC, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$ZoopAbund_m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
      dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprGenZ1 <- dplyr::bind_rows(cprGenZ1, genz)
  }

  cprGenZ1 <- cprGenZ1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Genus) %>%
    dplyr::summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$Genus)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenZ <-  cprGenZ1 %>%
    tidyr::pivot_wider(names_from = .data$Genus, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#' CPR Zoop copepod product - Abundance
#'
#' @return A dataframe with CPR Copepod Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooCopepod()
#' @importFrom rlang .data
pr_get_CPRZooCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData("Abundance")

  cprZcl <- pr_get_CPRZooChangeLog()%>%
    dplyr::mutate(same = dplyr::if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    dplyr::filter(.data$same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps("Z")

  # for non change log species

  cprCop1 <- cprZdat %>%
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName)) &
             .data$Copepod =="COPEPOD" &
             .data$Species != "spp." &
             !is.na(.data$Species) &
             .data$Species != '' &
             !grepl("cf.", .data$Species) &
             !grepl("/", .data$Species) &
             !grepl("grp", .data$Species)) %>%
    dplyr::mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  cprCop1 <- cprSamp %>%
    dplyr::left_join(cprCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = dplyr::if_else(is.na(.data$Species), "Calanus Australis", .data$Species), # avoids nulls in pivot
           ZoopAbund_m3 = dplyr::if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>% # avoids nulls in pivot
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprCop2 <- cprZdat %>%
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & .data$Copepod =="COPEPOD"
           & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)  &
             !grepl("/", .data$Species) & .data$Species != '') %>%
    dplyr::mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    dplyr::mutate(Species = forcats::as_factor(.data$Species)) %>% tidyr::drop_na(.data$Species) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprCop2$Species)) {
    Spe <- cprCop2 %>%
    dplyr::select(.data$Species) %>%
    unique()

    Spe <- as.character(Spe$Species[i] %>%
      droplevels())

    Dates <- as.data.frame(cprCop2) %>%
      dplyr::filter(.data$Species == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    copes <- as.data.frame(cprCop2) %>%
      dplyr::filter(.data$Species == Spe) %>%
      droplevels()

    copes <- cprSamp %>%
      dplyr::left_join(copes, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Species = replace(.data$Species, is.na(.data$Species), Dates$Species),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateUTC, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$ZoopAbund_m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
      dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprCop1 <- dplyr::bind_rows(cprCop1, copes)
  }

  cprCop1 <- cprCop1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$Species)) %>%
    as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprCop <- cprCop1 %>%
    tidyr::pivot_wider(names_from = .data$Species, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}

#' Get CPR Zoop Non-Copepod Abundance Data
#'
#' @return A dataframe with CPR Zooplankton (non-copepod) Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooNonCopepod()
#' @importFrom rlang .data
pr_get_CPRZooNonCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData("Abundance")

  cprZcl <- pr_get_CPRZooChangeLog() %>%
    dplyr::mutate(same = dplyr::if_else(.data$TaxonName == .data$ParentName, "yes", "no")) %>%
    dplyr::filter(.data$same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps("Z")

  # for non change logspecies
  cprnCop1 <- cprZdat %>%
    dplyr::filter(!.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & .data$Copepod !="COPEPOD"
          ) %>%
    pr_filter_species() %>%
    dplyr::mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  cprnCop1 <- cprSamp %>%
    dplyr::left_join(cprnCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
           Species = dplyr::if_else(is.na(.data$Species), "Evadne spinifera", .data$Species),
           ZoopAbund_m3 = dplyr::if_else(is.na(.data$ZoopAbund_m3), 0, .data$ZoopAbund_m3)) %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprnCop2 <- cprZdat %>%
    dplyr::filter(.data$TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & .data$Copepod !="COPEPOD"
           & .data$Species != "spp." & .data$Species != '' & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    dplyr::mutate(Species = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    tidyr::drop_na(.data$Species) %>%
    dplyr::mutate(Species = forcats::as_factor(.data$Species)) %>%
    dplyr::group_by(.data$Sample, .data$StartDate, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  for (i in 1:nlevels(cprnCop2$Species)) {
    Spe <- cprnCop2 %>%
    dplyr::select(.data$Species) %>%
    unique()

    Spe <- as.character(Spe$Species[i] %>%
      droplevels())

    Dates <- as.data.frame(cprnCop2) %>%
      dplyr::filter(.data$Species == Spe) %>%
      dplyr::slice(1) %>%
      droplevels()

    ncopes <- as.data.frame(cprnCop2) %>%
      dplyr::filter(.data$Species == Spe) %>%
      droplevels()

    ncopes <- cprSamp %>%
      dplyr::left_join(ncopes, by = "Sample") %>%
      dplyr::mutate(StartDate = replace(.data$StartDate, is.na(.data$StartDate), Dates$StartDate),
             Species = replace(.data$Species, is.na(.data$Species), Dates$Species),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate>.data$SampleDateUTC, -999),
             ZoopAbund_m3 = replace(.data$ZoopAbund_m3, .data$StartDate<.data$SampleDateUTC & is.na(.data$ZoopAbund_m3), 0)) %>%
      dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
      dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3), .groups = "drop") %>%
      as.data.frame()

    cprnCop1 <- dplyr::bind_rows(cprnCop1, ncopes)
  }

  cprnCop1 <- cprnCop1 %>%
    dplyr::group_by(.data$Latitude, .data$Longitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$Species) %>%
    dplyr::summarise(ZoopAbund_m3 = max(.data$ZoopAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(.data$Species)) %>% as.data.frame()
  # select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprnCop <-  cprnCop1 %>%
    tidyr::pivot_wider(names_from = .data$Species, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(.data$SampleDateUTC))
}
