#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- pr_get_indices_nrs()

pr_get_indices_nrs <- function(){
  # note there are circumstances where a trip won"t have a phyto and a zoo samples due to loss of sample etc.

  NRSdat <- pr_get_NRSTrips() %>%
    select(-SampleType) %>%
    filter(StationName != "Port Hacking 4") #ignore warning, "fast" method does better here than "accurate"

  dNRSdat <- distinct(NRSdat, TripCode, .keep_all = TRUE) %>% # Distinct rows for satellite, should be anyway
    pr_rename() %>%
    select(TripCode, Date, Latitude, Longitude)

  # SST and Chlorophyll from CTD
  CTD <- pr_get_CTD() %>%
    filter(SampleDepth_m < 15) %>% # take average of top 10m as a surface value for SST and CHL, this is removing 17 casts as of nov 2020
    group_by(TripCode) %>%
    summarise(CTDDensity_kgm3 = mean(WaterDensity_kgm3, na.rm = TRUE),
                     CTDTemperature = mean(Temperature_degC, na.rm = TRUE),
                     CTDConductivity_sm = mean(Conductivity_Sm, na.rm = TRUE),
                     CTDSalinity = mean(Salinity_psu, na.rm = TRUE),
                     CTDChlF_mgm3 = mean(Chla_mgm3, na.rm = TRUE),
                     CTDTurbidity_ntu = mean(Turbidity_NTU, na.rm = TRUE),
                     .groups = "drop")

  # Dataset for calculating MLD
  CTD_MLD <- pr_get_CTD() %>%
    select(TripCode, Temperature_degC, Chla_mgm3, Salinity_psu, SampleDepth_m) %>%
    # pr_rename() %>%
    rename(CTDTemperature = Temperature_degC, CTDSalinity = Salinity_psu, CTDChlF_mgm3 = Chla_mgm3) %>%
    tidyr::drop_na(TripCode)

  MLD <- data.frame(TripCode = character(), MLD_temp = numeric(), MLD_sal = numeric(), DCM = numeric())

  # MLD by T and S (Ref: Condie & Dunn 2006)
  # DCM from max f from CTD
  for (i in 1:length(unique(CTD_MLD$TripCode))) {
    dat <- CTD_MLD %>%
      select(TripCode) %>%
      distinct() %>%
      mutate(TripCode = as.factor(TripCode))

    Trip <- dat$TripCode[[i]] %>%
      droplevels()

    mldData <- CTD_MLD %>%
      filter(TripCode == Trip) %>%
      arrange(SampleDepth_m)

    if (as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 5
    }

    if (!as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 10
    }

    # Extract refence depth (z)
    refz <- mldData %>%
      mutate(refd = abs(SampleDepth_m - refDepth), # find depth nearest to 10 m
                    rankrefd = stats::ave(refd, FUN = . %>% order %>% order)) %>%
      filter(rankrefd == 1)

    # Reference Temperature
    refT <- refz$CTDTemperature - 0.4 # temp at 10 m minus 0.4 deg C

    mldData <- mldData %>%
      filter(SampleDepth_m > refz$SampleDepth_m)

    mld_t <- mldData %>%
      mutate(temp = abs(CTDTemperature - refT),
                    ranktemp = stats::ave(temp, FUN = . %>% order %>% order)) %>%
      filter(ranktemp == 1)

    MLD_temp <- mld_t$SampleDepth_m

    refS <- refz$CTDSalinity - 0.03 # temp at 10 m minus 0.4

    mld_s <- mldData %>%
      mutate(temp = abs(CTDSalinity - refS),
                    ranksal = stats::ave(temp, FUN = . %>% order %>% order)) %>%
      filter(ranksal == 1)

    MLD_sal <- mld_s$SampleDepth_m

    dcm <- (mldData %>%
              filter(CTDChlF_mgm3 > 0 & CTDChlF_mgm3 == max(CTDChlF_mgm3))
    )$SampleDepth_m
    dcm[rlang::is_empty(dcm)] = NA

    MLD <- MLD %>%
      bind_rows(data.frame(TripCode = as.character(Trip), MLD_temp = MLD_temp, MLD_sal = MLD_sal, DCM = dcm)) %>%
      tidyr::drop_na(TripCode)
  }

  # Nutrient data
  Nuts <- pr_get_Chemistry() %>%
    group_by(TripCode) %>%
    summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE),
                     Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
                     Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
                     Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
                     Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
                     Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
                     DIC_umolkg = mean(DIC_umolkg, na.rm = TRUE),
                     TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
                     Salinity_psu = mean(Salinity_psu, na.rm = TRUE),
                     .groups = "drop") %>%
    mutate_all(~ replace(., is.na(.), NA))

  Pigments <- pr_get_NRSPigments() %>%
    filter(SampleDepth_m <= 25) %>% # take average of top 10m as a surface value for SST and CHL
    # filter(SampleDepth_m == "WC") %>%
    group_by(TripCode) %>%
    summarise(Chla_mgm3 = mean(DV_CPHL_A_AND_CPHL_A, na.rm = TRUE),
                     .groups = "drop")

  # Total Zooplankton Abundance
  ZooData <- pr_get_NRSTrips() %>%
    left_join(pr_get_NRSZooData(), by = "TripCode")

  TZoo <- ZooData %>%
    group_by(TripCode) %>%
    summarise(ZoopAbundance_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  TCope <- ZooData %>%
    filter(Copepod == "COPEPOD") %>%
    group_by(TripCode, ) %>%
    summarise(CopeAbundance_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  # Bring in copepod information table with sizes etc.
  ZInfo <- pr_get_ZooInfo()

  ACopeSize <- ZooData %>%
    filter(Copepod == "COPEPOD") %>%
    inner_join(ZInfo %>%
                        select(LENGTH_MM, TaxonName, DIET), by = "TaxonName") %>%
    mutate(abunSize = LENGTH_MM * ZooPhytoAbund_m3,
                  DIET = ifelse(DIET == "CC", "CC", "CO")) %>%
    group_by(TripCode) %>%
    summarise(AvgTotalLengthCopepod_mm = sum(abunSize, na.rm = TRUE)/sum(ZooPhytoAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  HCrat <- ZooData %>% #TODO This whole section needs to be reconsidered. Not sure it gives the correct ratios
    filter(Copepod == "COPEPOD") %>%
    inner_join(ZInfo %>%
                        select(TaxonName, DIET), by = "TaxonName") %>%
    mutate(DIET = case_when(
      DIET == "Carnivore" ~ "CC",
      DIET == "Omnivore" ~ "CO",
      DIET == "Herbivore" ~ "CO")) %>% #TODO Check that Herbivore is correct
    tidyr::drop_na() %>%
    select(TripCode, DIET, ZooPhytoAbund_m3) %>%
    group_by(TripCode, DIET) %>%
    summarise(sumdiet = sum(ZooPhytoAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumdiet, names_from = DIET) %>%
    mutate(HerbivoreCarnivoreCopepodRatio = CO / (CO + CC))

  # Diversity, evenness etc.

  # Bring in plankton data
  ZooCount <- pr_get_NRSTrips() %>%
    # left_join(pr_get_NRSZooCount(), by = "TripCode")
    left_join(pr_get_NRSZooData(), by = "TripCode")

  zoo_n <- ZooCount %>%
    filter(Copepod == "COPEPOD" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode) %>%
    summarise(NoCopepodSpecies_Sample = n(), .groups = "drop")

  ShannonCopepodDiversity <- ZooCount %>%
    filter(Copepod == "COPEPOD" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, TaxonName) %>%
    summarise(ZCount = sum(TaxonCount, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = ZCount, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-TripCode) %>%
    vegan::diversity("shannon")

  CopepodEvenness <- zoo_n %>%
    bind_cols(ShannonCopepodDiversity = ShannonCopepodDiversity) %>%
    mutate(CopepodEvenness = ShannonCopepodDiversity / log(NoCopepodSpecies_Sample))

  # Total Phyto abundance
  PhytoData <- pr_get_NRSTrips() %>%
    left_join(pr_get_NRSPhytoData(), by = "TripCode") %>%
    filter(TaxonGroup != "Other")

  # PhytoData <- PhytoData %>%
  #  filter(str_detect(TaxonName, "Flagellate <10", negate = TRUE)) # Remove flagellates #TODO

  PhytoC <- PhytoData %>%
    select(TripCode, TaxonGroup, Cells_L, Biovolume_um3L) %>%
    mutate(BV_Cell = Biovolume_um3L / Cells_L, # biovolume of one cell
                  Carbon = ifelse(TaxonGroup == "Dinoflagellate", 0.76*(BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                  ifelse(TaxonGroup == "Ciliate", 0.22*(BV_Cell)^0.939,
                                         ifelse(TaxonGroup == "Cyanobacteria", 0.2, 0.288*(BV_Cell)^0.811 ))),
                  Carbon_L = Cells_L * Carbon) %>% # Carbon per litre
    group_by(TripCode) %>%
    summarise(PhytoBiomassCarbon_pg_L = sum(Carbon_L),
                     .groups = "drop")

  TPhyto <- PhytoData %>%
    group_by(TripCode) %>%
    summarise(AbundancePhyto_cells_L = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop")

  DDrat <- PhytoData %>%
    filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom", "Dinoflagellate")) %>%
    mutate(TaxonGroup = recode(TaxonGroup, "Centric diatom" = "Diatom", "Pennate diatom" = "Diatom")) %>%
    select(TripCode, TaxonGroup, Cells_L) %>%
    group_by(TripCode, TaxonGroup) %>%
    summarise(sumTG = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumTG, names_from = TaxonGroup) %>%
    mutate(DiatomDinoflagellateRatio = Diatom / (Diatom + Dinoflagellate))

  AvgCellVol <- PhytoData %>%
    filter(!is.na(Biovolume_um3L)) %>%
    group_by(TripCode) %>%
    summarise(AvgCellVol_um3 = mean(sum(Biovolume_um3L)/sum(Cells_L)),
                     .groups = "drop")

  # vegan::diversity (phyto, diatoms, dinos)
  # stick to abundance data here or we lose all the data that Pru counted which we don"t have counts for.

  NP <- PhytoData %>%
    filter(TaxonGroup != "Other" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode) %>%
    summarise(NoPhytoSpecies_Sample = n(),
                     .groups = "drop")

  ShannonPhytoDiversity <- PhytoData %>%
    filter(TaxonGroup != "Other" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, TaxonName) %>%
    summarise(Pdata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Pdata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-TripCode) %>%
    vegan::diversity("shannon")

  PhytoEven <- NP %>%
    bind_cols(ShannonPhytoDiversity = ShannonPhytoDiversity) %>%
    mutate(PhytoEvenness = ShannonPhytoDiversity / log(NoPhytoSpecies_Sample))

  NDia <- PhytoData %>%
    filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom") & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode) %>%
    summarise(NoDiatomSpecies_Sample = n(),
                     .groups = "drop")

  ShannonDiatomDiversity <- PhytoData %>%
    filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom") & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, TaxonName) %>%
    summarise(Diadata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Diadata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-TripCode) %>%
    vegan::diversity("shannon")

  DiaEven <- NDia %>%
    bind_cols(ShannonDiatomDiversity = ShannonDiatomDiversity) %>%
    mutate(DiatomEvenness = ShannonDiatomDiversity / log(NoDiatomSpecies_Sample))

  NDino <- PhytoData %>%
    filter(TaxonGroup == "Dinoflagellate" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode) %>%
    summarise(NoDinoSpecies_Sample = n(),
                     .groups = "drop")

  ShannonDinoDiversity <- PhytoData %>%
    filter(TaxonGroup == "Dinoflagellate" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(TripCode, TaxonName) %>%
    summarise(Dinodata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Dinodata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-TripCode) %>%
    vegan::diversity("shannon")

  DinoEven <- NDino %>%
    bind_cols(ShannonDiatomDiversity = ShannonDinoDiversity) %>%
    mutate(DinoflagellateEvenness = ShannonDinoDiversity / log(NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  Indices <- NRSdat %>%
    left_join(TZoo, by = ("TripCode")) %>%
    left_join(TCope, by = ("TripCode")) %>%
    left_join(ACopeSize, by = ("TripCode")) %>%
    left_join(HCrat %>%
                       select(-c("CO", "CC")), ("TripCode")) %>%
    left_join(CopepodEvenness, by = ("TripCode")) %>%
    left_join(PhytoC, by = ("TripCode")) %>%
    left_join(TPhyto, by = ("TripCode")) %>%
    left_join(DDrat %>% select(-c("Diatom", "Dinoflagellate")), by = ("TripCode")) %>%
    left_join(AvgCellVol, by = ("TripCode")) %>%
    left_join(PhytoEven, by = ("TripCode")) %>%
    left_join(DiaEven, by = ("TripCode")) %>%
    left_join(DinoEven, by = ("TripCode")) %>%
    left_join(CTD, by = ("TripCode")) %>%
    left_join(MLD, by = ("TripCode")) %>%
    left_join(Nuts, by = ("TripCode")) %>%
    left_join(Pigments, by = ("TripCode"))

  return(Indices)
}
