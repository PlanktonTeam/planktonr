#' Get CPR trips
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRTrips()
#' @importFrom magrittr "%>%"
pr_get_CPRTrips <- function(){
  CPRTrips <- readr::read_csv(paste0(pr_get_site(), "CPR_Trips.csv"), na = "") %>%
    pr_rename()
}

#' Get CPR samples
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRSamps()
#' @importFrom magrittr "%>%"
pr_get_CPRSamps <- function(){
  CPRSamps <- readr::read_csv(paste0(pr_get_site(), "CPR_Samp.csv"), na = "") %>%
    pr_rename() %>%
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
#' df <- pr_get_CPRPhytoData()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoData <- function(){
  cprPdat <- readr::read_csv(paste0(pr_get_site(), "CPR_Phyto_Raw.csv"), na = "") %>%
    pr_rename() %>%
    dplyr::select(-c(FovCount, SampVol_m3))
}

#' Get CPR Phytoplankton Count data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoCountData()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoCountData <- function(){
  cprPdat <- readr::read_csv(paste0(pr_get_site(), "CPR_Phyto_Raw.csv"), na = "") %>%
    pr_rename() %>%
    dplyr::select(-c(BioVolume_um3m3, PhytoAbund_m3))
}

#' Get Phyto Change Log
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoChangeLog()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoChangeLog <- function(){
  cprPcl <- readr::read_csv(paste0(pr_get_site(), "CPR_Phyto_ChangeLog.csv"), na = "") %>%
    pr_rename()
}

#' Get CPR Zooplankton Count
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooCountData()
#' @importFrom magrittr "%>%"
pr_get_CPRZooCountData <- function() {
  CPRZooCount <- readr::read_csv(paste0(pr_get_site(), "CPR_Zoop_Raw.csv"), na = "(null)") %>%
    pr_rename() %>%
    dplyr::select(-ZooPhytoAbund_m3)
}

#' Get CPR Zooplankton abundance data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooData()
#' @importFrom magrittr "%>%"
pr_get_CPRZooData <- function(){
  cprZdat <- readr::read_csv(paste0(pr_get_site(), "CPR_Zoop_Raw.csv"), na = "") %>%
    pr_rename() %>%
    dplyr::select(-c(TaxonCount, SampVol_m3))
}

#' Get CPR zooplankton Change Log
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooChangeLog()
#' @importFrom magrittr "%>%"
pr_get_CPRZooChangeLog <- function(){
  cprZcl <- readr::read_csv(paste0(pr_get_site(), "CPR_Zoop_ChangeLog.csv"), na = "") %>%
    pr_rename()
}

################################################################################################################################################
#' Get CPR Phyto raw & pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRaw()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoRaw <- function(){
  cprRawP <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(pr_get_CPRPhytoData(), by = "Sample") %>%
    dplyr::select(c(Sample:TaxonName,PhytoAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    dplyr::mutate(TaxonName = ifelse(is.na(TaxonName), "No taxa found", TaxonName)) %>% # for segments where no phyto was found
    tidyr::pivot_wider(names_from = TaxonName, values_from = PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-"No taxa found")
}

#' CPR Phyto HTG pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoHTG
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoHTG <- function(){
  cprHTGP1 <- pr_get_CPRPhytoData() %>% dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGP <-  pr_get_CPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>% dplyr::left_join(cprHTGP1, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Ciliate", TaxonGroup),
                  PhytoAbund_m3 = ifelse(is.na(PhytoAbund_m3), 0, PhytoAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-Sample)
}

#' Get CPR Phyto genus pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoGenus()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData()

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species

  cprGenP1 <- cprPdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::group_by(Sample, Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!Genus == "")

  cprGenP1 <- cprSamp %>%
    dplyr::left_join(cprGenP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Acanthoica", Genus),
                  PhytoAbund_m3 = ifelse(is.na(PhytoAbund_m3), 0, PhytoAbund_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenP2 <- cprPdat  %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprPcl$TaxonName))) %>%
    dplyr::left_join(cprPcl, by = "TaxonName") %>%
    dplyr::filter(Genus != '') %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    tidyr::drop_na(Genus) %>%
    dplyr::group_by(Sample, StartDate, Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

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
                    PhytoAbund_m3 = replace(PhytoAbund_m3, StartDate>SampleDateUTC, -999),
                    PhytoAbund_m3 = replace(PhytoAbund_m3, StartDate<SampleDateUTC & is.na(PhytoAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()

    cprGenP1 <- dplyr::bind_rows(cprGenP1, gen)
  }

  cprGenP1 <- cprGenP1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(PhytoAbund_m3 = max(PhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenP <-  cprGenP1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Phyto species pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoSpecies()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoSpecies <-  function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData()

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species
  cprSpecP1 <- cprPdat %>%
    dplyr::mutate(TaxonName = paste0(Genus, ' ', Species)) %>% # remove comments about with flagellates or ciliates etc.
    dplyr::filter(!TaxonName %in% levels(as.factor(cprPcl$TaxonName))
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species)) %>%
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  cprSpecP1 <- cprSamp %>%
    dplyr::left_join(cprSpecP1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  TaxonName = ifelse(is.na(TaxonName), "Paralia sulcata", TaxonName),
                  PhytoAbund_m3 = ifelse(is.na(PhytoAbund_m3), 0, PhytoAbund_m3))  %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3), .groups = "drop") %>%
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
    dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop")

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
                    PhytoAbund_m3 = replace(PhytoAbund_m3, StartDate>SampleDateUTC, -999),
                    PhytoAbund_m3 = replace(PhytoAbund_m3, StartDate<SampleDateUTC & is.na(PhytoAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
      dplyr::summarise(PhytoAbund_m3 = sum(PhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprSpecP1 <- dplyr::bind_rows(cprSpecP1, spec)
  }

  cprSpecP1 <- cprSpecP1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(PhytoAbund_m3 = max(PhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    as.data.frame()

  # dplyr::select maximum value of duplicates, but leave -999 for all other occurences as not regularly identified
  cprSpecP <-  cprSpecP1 %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = PhytoAbund_m3, values_fill = list(PhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

###############################################################################################################################################
#' Get CPR Phyto raw & pivoted product - Biovolume
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRawBV()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoRawBV <- function(){
  cprRawP <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(pr_get_CPRPhytoData(), by = "Sample") %>%
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
#' df <- pr_get_CPRHTGBV()
#' @importFrom magrittr "%>%"
pr_get_CPRHTGBV <- function(){
  cprHTGPB1 <- pr_get_CPRPhytoData() %>%
    dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(PBioV_um3m3 = sum(BioVolume_um3m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other","Coccolithophore", "Diatom","Protozoa"))

  cprHTGPB1 <-  pr_get_CPRSamps()  %>%
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
#' df <- pr_get_CPRPhytoGenusBV()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoGenusBV <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData()

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
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
#' df <- pr_get_CPRPhytoSpeciesBV()
#' @importFrom magrittr "%>%"
pr_get_CPRPhytoSpeciesBV <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprPdat <- pr_get_CPRPhytoData()

  cprPcl <- pr_get_CPRPhytoChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
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
#' df <- pr_get_CPRZooRaw()
#' @importFrom magrittr "%>%"
pr_get_CPRZooRaw <- function(){
  cprRawZ <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(pr_get_CPRZooData(), by = "Sample") %>%
    dplyr::select(-c("Copepod", "TaxonGroup", "Genus", "Species", 'SPCode')) %>%
    dplyr::arrange(-dplyr::desc(TaxonName)) %>%
    dplyr::mutate(TaxonName = ifelse(is.na(TaxonName), "No taxa found", TaxonName)) %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
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
#' df <- pr_get_CPRZooRawSS()
#' @importFrom magrittr "%>%"
pr_get_CPRZooRawSS <- function(){
  CPRIdsZ <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(pr_get_CPRZooData(), by = "Sample") %>%
    dplyr::mutate(TaxonName = ifelse(is.na(Genus), TaxonName, paste0(Genus, ' ', Species))) %>%
    dplyr::group_by(TripCode, Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, TaxonName) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE)) %>%
    dplyr::arrange(-dplyr::desc(TaxonName))  %>%
    tidyr::pivot_wider(names_from = TaxonName, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Zoop HTG pivoted product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooHTG()
#' @importFrom magrittr "%>%"
pr_get_CPRZooHTG <- function(){
  cprHTGZ <- pr_get_CPRZooData() %>%
    dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!TaxonGroup %in% c("Other"))

  cprHTGZ1 <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3)) %>%
    dplyr::left_join(cprHTGZ, by = "Sample") %>%
    dplyr::mutate(TaxonGroup = ifelse(is.na(TaxonGroup), "Copepod", TaxonGroup),
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%
    dplyr::arrange(-dplyr::desc(TaxonGroup)) %>%
    tidyr::pivot_wider(names_from = TaxonGroup, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC)) %>%
    dplyr::select(-Sample)
}

#' CPR Zoop genus product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooGenus()
#' @importFrom magrittr "%>%"
pr_get_CPRZooGenus <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData()

  cprZcl <- pr_get_CPRZooChangeLog() %>%
    dplyr::mutate(genus1 = stringr::word(TaxonName, 1),
                  genus2 = stringr::word(ParentName, 1)) %>%
    dplyr::mutate(same = ifelse(genus1==genus2, "yes", "no")) %>%
    dplyr::filter(same == "no")# no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change log species
  cprGenZ1 <- cprZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::group_by(Sample, Genus) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(Genus != '')

  cprGenZ1 <- cprSamp %>%
    dplyr::left_join(cprGenZ1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Genus = ifelse(is.na(Genus), "Calanus", Genus),
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
    as.data.frame()

  # add change log species with -999 for NA"s and real absences as 0"s
  cprGenZ2 <- cprZdat %>%
    dplyr::filter(TaxonName %in% levels(as.factor(cprZcl$TaxonName))) %>%
    dplyr::left_join(cprZcl, by = "TaxonName") %>%
    dplyr::filter(Genus != '')  %>%
    dplyr::mutate(Genus = forcats::as_factor(Genus)) %>%
    dplyr::group_by(Sample, StartDate, Genus) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

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
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateUTC, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateUTC & is.na(ZooPhytoAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
      dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprGenZ1 <- dplyr::bind_rows(cprGenZ1, genz)
  }

  cprGenZ1 <- cprGenZ1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Genus) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Genus)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprGenZ <-  cprGenZ1 %>%
    tidyr::pivot_wider(names_from = Genus, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' CPR Zoop copepod product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooCopepod()
#' @importFrom magrittr "%>%"
pr_get_CPRZooCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData()

  cprZcl <- pr_get_CPRZooChangeLog()%>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
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
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  cprCop1 <- cprSamp %>%
    dplyr::left_join(cprCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Calanus Australis", Species), # avoids nulls in pivot
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>% # avoids nulls in pivot
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
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
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

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
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateUTC, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateUTC & is.na(ZooPhytoAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprCop1 <- dplyr::bind_rows(cprCop1, copes)
  }

  cprCop1 <- cprCop1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>%
    as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprCop <- cprCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}

#' Get CPR Zoop copepod product - Abundance
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooNonCopepod()
#' @importFrom magrittr "%>%"
pr_get_CPRZooNonCopepod <- function(){
  # Bring in all NRS zooplankton samples, data and changelog once
  cprZdat <- pr_get_CPRZooData()

  cprZcl <- pr_get_CPRZooChangeLog() %>%
    dplyr::mutate(same = ifelse(TaxonName == ParentName, "yes", "no")) %>%
    dplyr::filter(same == "no") # no changes at genera level

  cprSamp <- pr_get_CPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  # for non change logspecies
  cprnCop1 <- cprZdat %>%
    dplyr::filter(!TaxonName %in% levels(as.factor(cprZcl$TaxonName)) & Copepod !="COPEPOD"
                  & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(Species = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, Species) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  cprnCop1 <- cprSamp %>%
    dplyr::left_join(cprnCop1, by = "Sample") %>%
    dplyr::mutate(StartDate = lubridate::ymd("2007-12-19"),
                  Species = ifelse(is.na(Species), "Evadne spinifera", Species),
                  ZooPhytoAbund_m3 = ifelse(is.na(ZooPhytoAbund_m3), 0, ZooPhytoAbund_m3)) %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
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
    dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

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
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate>SampleDateUTC, -999),
                    ZooPhytoAbund_m3 = replace(ZooPhytoAbund_m3, StartDate<SampleDateUTC & is.na(ZooPhytoAbund_m3), 0)) %>%
      dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
      dplyr::summarise(ZooPhytoAbund_m3 = sum(ZooPhytoAbund_m3), .groups = "drop") %>%
      as.data.frame()
    cprnCop1 <- dplyr::bind_rows(cprnCop1, ncopes)
  }

  cprnCop1 <- cprnCop1 %>%
    dplyr::group_by(Latitude, Longitude, SampleDateUTC, Year, Month, Day, Time_24hr, Species) %>%
    dplyr::summarise(ZooPhytoAbund_m3 = max(ZooPhytoAbund_m3), .groups = "drop") %>%
    dplyr::arrange(-dplyr::desc(Species)) %>% as.data.frame()
  # dplyr::select maximum value of duplicates, but leave -999 for all other occurrences as not regularly identified

  cprnCop <-  cprnCop1 %>%
    tidyr::pivot_wider(names_from = Species, values_from = ZooPhytoAbund_m3, values_fill = list(ZooPhytoAbund_m3 = 0)) %>%
    dplyr::arrange(dplyr::desc(SampleDateUTC))
}
