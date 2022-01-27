#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- pr_get_indices_nrs()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_indices_nrs <- function(){
  # note there are circumstances where a trip won"t have a phyto and a zoo samples due to loss of sample etc.

  NRSdat <- pr_get_NRSTrips(c("P", "Z", "F")) %>%
    select(-c(.data$SampleType, .data$Methods)) %>%
    dplyr::mutate(SampleDateLocal = strptime(.data$SampleDateLocal, format = "%Y-%m-%d")) %>%
    filter(.data$StationName != "Port Hacking 4") %>%
    dplyr::select(-c(ZSampleDepth_m, PSampleDepth_m, SampleDateUTC))

  dNRSdat <- distinct(NRSdat, .data$TripCode, .keep_all = TRUE) %>% # Distinct rows for satellite, should be anyway
    pr_rename() %>%
    select(.data$TripCode, .data$Date, .data$Latitude, .data$Longitude)

  var_names <- c("Temperature_degC", "Salinity_psu", "ChlF_mgm3")
  # SST and Chlorophyll from CTD
  CTD <- pr_get_CTD() %>%
    pr_rename() %>%
    filter(.data$SampleDepth_m < 15) %>% # take average of top 10m as a surface value for SST and CHL, this is removing 17 casts as of nov 2020
    group_by(.data$TripCode) %>%
    summarise(across(matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::rename(CTDTemperature_degC = Temperature_degC, CTDChlF_mgm3 = ChlF_mgm3, CTDSalinity_psu = Salinity_psu)

  # Dataset for calculating MLD
  CTD_MLD <- pr_get_CTD() %>%
    select(.data$TripCode, .data$Temperature_degC, .data$ChlF_mgm3, .data$Salinity_psu, .data$SampleDepth_m) %>%
    # pr_rename() %>%
    # rename(CTDTemperature = .data$Temperature_degC, CTDSalinity = .data$Salinity_psu, CTDChlF_mgm3 = .data$Chla_mgm3) %>%
    tidyr::drop_na(.data$TripCode)

  MLD <- data.frame(TripCode = character(), MLDtemp_m = numeric(), MLDsal_m = numeric(), DCM_m = numeric())

  # MLD by T and S (Ref: Condie & Dunn 2006)
  # DCM from max f from CTD
  for (i in 1:length(unique(CTD_MLD$TripCode))) {
    dat <- CTD_MLD %>%
      select(.data$TripCode) %>%
      distinct() %>%
      mutate(TripCode = as.factor(.data$TripCode))

    Trip <- dat$TripCode[[i]] %>%
      droplevels()

    mldData <- CTD_MLD %>%
      filter(.data$TripCode == Trip) %>%
      arrange(.data$SampleDepth_m)

    if (as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 5
    }

    if (!as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 10
    }

    # Extract refence depth (z)
    refz <- mldData %>%
      mutate(refd = abs(.data$SampleDepth_m - refDepth), # find depth nearest to 10 m
             rankrefd = stats::ave(.data$refd, FUN = . %>% order %>% order)) %>%
      filter(.data$rankrefd == 1)

    # Reference Temperature
    refT <- refz$Temperature_degC - 0.4 # temp at 10 m minus 0.4 deg C

    mldData <- mldData %>%
      filter(.data$SampleDepth_m > refz$SampleDepth_m)

    mld_t <- mldData %>%
      mutate(temp = abs(.data$Temperature_degC - refT),
             ranktemp = stats::ave(.data$temp, FUN = . %>% order %>% order)) %>%
      filter(.data$ranktemp == 1)

    MLDtemp_m <- mld_t$SampleDepth_m

    refS <- refz$Salinity_psu - 0.03 # temp at 10 m minus 0.4

    mld_s <- mldData %>%
      mutate(temp = abs(.data$Salinity_psu - refS),
             ranksal = stats::ave(.data$temp, FUN = . %>% order %>% order)) %>%
      filter(.data$ranksal == 1)

    MLDsal_m <- mld_s$SampleDepth_m

    dcm_m <- mean((mldData %>%
              filter(.data$ChlF_mgm3 > 0 & .data$ChlF_mgm3 == max(.data$ChlF_mgm3))
    )$SampleDepth_m)
    dcm_m[rlang::is_empty(dcm_m)] = NA

    MLD <- MLD %>%
      bind_rows(data.frame(TripCode = as.character(Trip), MLDtemp_m = MLDtemp_m, MLDsal_m = MLDsal_m, DCM_m = dcm_m)) %>%
      tidyr::drop_na(.data$TripCode)
  }

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "DIC_umolkg", "TAlkalinity_umolkg", "Salinity_psu")
  # Nutrient data
  Nuts <- pr_get_Chemistry() %>%
    group_by(.data$TripCode) %>%
    summarise(across(matches(var_names), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate_all(~ replace(., is.na(.), NA))

  Pigments <- pr_get_NRSPigments() %>%
    filter(.data$SampleDepth_m <= 25) %>% # take average of top 10m as a surface value for SST and CHL
    # filter(.data$SampleDepth_m == "WC") %>%
    group_by(.data$TripCode) %>%
    summarise(PigmentChla_mgm3 = mean(.data$DV_CPHL_A_AND_CPHL_A, na.rm = TRUE),
              .groups = "drop")

  # Total Zooplankton Abundance
  ZooData <- pr_get_NRSTrips("Z") %>%
    left_join(pr_get_NRSZooData(), by = "TripCode")

  TZoo <- ZooData %>%
    group_by(.data$TripCode) %>%
    tidyr::drop_na(.data$ZoopAbund_m3) %>% # stops code putting 0 for trip codes with no counts when na.rm = TRUE
    summarise(ZoopAbundance_m3 = sum(.data$ZoopAbund_m3),
              .groups = "drop")

  TCope <- ZooData %>%
    filter(.data$Copepod == "COPEPOD") %>%
    group_by(.data$TripCode, ) %>%
    summarise(CopeAbundance_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE),
              .groups = "drop")

  # Bring in copepod information table with sizes etc.
  ZInfo <- pr_get_ZooInfo()

  ACopeSize <- ZooData %>%
    filter(.data$Copepod == "COPEPOD") %>%
    inner_join(ZInfo %>%
                 select(.data$Length_mm, .data$TaxonName, .data$Diet), by = "TaxonName") %>%
    mutate(abunSize = .data$Length_mm * .data$ZoopAbund_m3,
           Diet = if_else(.data$Diet == "CC", "CC", "CO")) %>%
    group_by(.data$TripCode) %>%
    summarise(AvgTotalLengthCopepod_mm = sum(.data$abunSize, na.rm = TRUE)/sum(.data$ZoopAbund_m3, na.rm = TRUE),
              .groups = "drop")

  HCrat <- ZooData %>% #TODO This whole section needs to be reconsidered. Not sure it gives the correct ratios
    filter(.data$Copepod == "COPEPOD") %>%
    inner_join(ZInfo %>%
                 select(.data$TaxonName, .data$Diet), by = "TaxonName") %>%
    mutate(Diet = case_when(
      .data$Diet == "Carnivore" ~ "CC",
      .data$Diet == "Omnivore" ~ "CO",
      .data$Diet == "Herbivore" ~ "CO")) %>% #TODO Check that Herbivore is correct
    tidyr::drop_na() %>%
    select(.data$TripCode, .data$Diet, .data$ZoopAbund_m3) %>%
    group_by(.data$TripCode, .data$Diet) %>%
    summarise(sumdiet = sum(.data$ZoopAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumdiet, names_from = .data$Diet) %>%
    mutate(HerbivoreCarnivoreCopepodRatio = .data$CO / (.data$CO + .data$CC))

  # Diversity, evenness etc.

  # Bring in plankton data
  ZooCount <- pr_get_NRSTrips("Z") %>%
    left_join(pr_get_NRSZooData(), by = "TripCode")

  zoo_n <- ZooCount %>%
    filter(.data$Copepod == "COPEPOD") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$TripCode, .data$TaxonName) %>%
    unique() %>%
    group_by(.data$TripCode) %>%
    summarise(NoCopepodSpecies_Sample = n(), .groups = "drop")

  ShannonCopepodDiversity <- ZooCount %>%
    filter(.data$Copepod == "COPEPOD") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(ZCount = sum(.data$TaxonCount, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$ZCount, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$TripCode) %>%
    vegan::diversity("shannon")

  CopepodEvenness <- zoo_n %>%
    bind_cols(ShannonCopepodDiversity = ShannonCopepodDiversity) %>%
    mutate(CopepodEvenness = .data$ShannonCopepodDiversity / log(.data$NoCopepodSpecies_Sample))

  # Total Phyto abundance
  PhytoData <- pr_get_NRSTrips("P") %>%
    left_join(pr_get_NRSPhytoData(), by = "TripCode") %>%
    filter(.data$TaxonGroup != "Other")

  # PhytoData <- PhytoData %>%
  #  filter(str_detect(.data$TaxonName, "Flagellate <10", negate = TRUE)) # Remove flagellates #TODO

  PhytoC <- PhytoData %>%
    select(.data$TripCode, .data$TaxonGroup, .data$Cells_L, .data$Biovolume_um3L) %>%
    pr_add_Carbon("NRS") %>% # Add carbon concentration
    group_by(.data$TripCode) %>%
    summarise(PhytoBiomassCarbon_pgL = sum(.data$Carbon_L),
              .groups = "drop")

  TPhyto <- PhytoData %>%
    group_by(.data$TripCode) %>%
    summarise(PhytoAbund_CellsL = sum(.data$Cells_L, na.rm = TRUE),
              .groups = "drop")

  DDrat <- PhytoData %>%
    filter(.data$TaxonGroup %in% c("Centric diatom", "Pennate diatom", "Dinoflagellate")) %>%
    mutate(TaxonGroup = recode(.data$TaxonGroup, "Centric diatom" = "Diatom", "Pennate diatom" = "Diatom")) %>%
    select(.data$TripCode, .data$TaxonGroup, .data$Cells_L) %>%
    group_by(.data$TripCode, .data$TaxonGroup) %>%
    summarise(sumTG = sum(.data$Cells_L, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumTG, names_from = .data$TaxonGroup) %>%
    mutate(DiatomDinoflagellateRatio = .data$Diatom / (.data$Diatom + .data$Dinoflagellate))

  AvgCellVol <- PhytoData %>%
    filter(!is.na(.data$Biovolume_um3L)) %>%
    group_by(.data$TripCode) %>%
    summarise(AvgCellVol_um3 = mean(sum(.data$Biovolume_um3L)/sum(.data$Cells_L)),
              .groups = "drop")

  # vegan::diversity (phyto, diatoms, dinos)
  # stick to abundance data here or we lose all the data that Pru counted which we don"t have counts for.

  NP <- PhytoData %>%
    filter(.data$TaxonGroup != "Other") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$TaxonName, .data$TripCode) %>%
    unique() %>%
    group_by(.data$TripCode) %>%
    summarise(NoPhytoSpecies_Sample = n(),
              .groups = "drop")

  ShannonPhytoDiversity <- PhytoData %>%
    filter(.data$TaxonGroup != "Other") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(Pdata = sum(.data$Cells_L, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Pdata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$TripCode) %>%
    vegan::diversity("shannon")

  PhytoEven <- NP %>%
    bind_cols(ShannonPhytoDiversity = ShannonPhytoDiversity) %>%
    mutate(PhytoEvenness = .data$ShannonPhytoDiversity / log(.data$NoPhytoSpecies_Sample))

  NDia <- PhytoData %>%
    filter(.data$TaxonGroup %in% c("Centric diatom", "Pennate diatom")) %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode) %>%
    summarise(NoDiatomSpecies_Sample = n(),
              .groups = "drop")

  ShannonDiatomDiversity <- PhytoData %>%
    filter(.data$TaxonGroup %in% c("Centric diatom", "Pennate diatom")) %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(Diadata = sum(.data$Cells_L, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Diadata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$TripCode) %>%
    vegan::diversity("shannon")

  DiaEven <- NDia %>%
    bind_cols(ShannonDiatomDiversity = ShannonDiatomDiversity) %>%
    mutate(DiatomEvenness = .data$ShannonDiatomDiversity / log(.data$NoDiatomSpecies_Sample))

  NDino <- PhytoData %>%
    filter(.data$TaxonGroup == "Dinoflagellate") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode) %>%
    summarise(NoDinoSpecies_Sample = n(),
              .groups = "drop")

  ShannonDinoDiversity <- PhytoData %>%
    filter(.data$TaxonGroup == "Dinoflagellate") %>%
    pr_filter_species() %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$TripCode, .data$TaxonName) %>%
    summarise(Dinodata = sum(.data$Cells_L, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Dinodata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$TripCode) %>%
    vegan::diversity("shannon")

  DinoEven <- NDino %>%
    bind_cols(ShannonDinoDiversity = ShannonDinoDiversity) %>%
    mutate(DinoflagellateEvenness = .data$ShannonDinoDiversity / log(.data$NoDinoSpecies_Sample))

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
    left_join(Pigments, by = ("TripCode")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), ""))

}

