#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- pr_get_indices_cpr()
#'
#' @importFrom rlang .data
pr_get_indices_cpr <- function(){

  # Add the bioregions to the CPR data
  cprSampleInfo <- pr_get_CPRSamps(c("P","Z","B")) %>%
    dplyr::select(.data$TripCode:.data$Time_24hr, .data$BioRegion, .data$Biomass_mgm3)

  cprZsamp <- pr_get_CPRSamps(c("Z", "B"))

  cprZdat <- pr_get_CPRZooData()

  # Total zoop abundance
  zoodatacpr <- cprZsamp %>%
    dplyr::left_join(cprZdat, by = "Sample")

  TZoocpr <- zoodatacpr %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  TCopecpr <- zoodatacpr %>%
    dplyr::filter(.data$Copepod == 'COPEPOD') %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(CopeAbundance_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  # Bring in copepod information table with sizes etc.
  Zinfo <- pr_get_ZooInfo()

  ACopeSizeCpr <- zoodatacpr %>%
    dplyr::filter(.data$Copepod == 'COPEPOD') %>%
    dplyr::inner_join(Zinfo %>% dplyr::select(.data$Length_mm, .data$TaxonName, .data$Diet), by = "TaxonName") %>%
    dplyr::mutate(abunSize = .data$Length_mm * .data$ZoopAbund_m3) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(AvgTotalLengthCopepod_mm = sum(.data$abunSize, na.rm = TRUE)/sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  HCratCpr <- zoodatacpr %>%
    dplyr::filter(.data$Copepod == 'COPEPOD') %>%
    dplyr::inner_join(Zinfo %>% dplyr::select(.data$TaxonName, .data$Diet), by = "TaxonName") %>%
    dplyr::mutate(Diet = dplyr::if_else(.data$Diet == 'Herbivore', 'Omnivore', .data$Diet)) %>%
    dplyr::filter(.data$Diet != 'unknown') %>%
    dplyr::select(.data$Sample, .data$Diet, .data$ZoopAbund_m3) %>%
    dplyr::group_by(.data$Sample, .data$Diet) %>%
    dplyr::summarise(sumdiet = sum(.data$ZoopAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumdiet, names_from = .data$Diet) %>%
    dplyr::mutate(HerbivoreCarnivoreCopepodRatio = .data$Carnivore / (.data$Omnivore + .data$Carnivore))

  # Diversity, evenness etc.

  # Bring in plankton data
  CPRZcount <- pr_get_CPRZooData("Count")

  zooCountCpr <- cprZsamp %>% # Changed this from cprtr
    dplyr::left_join(CPRZcount, by = "Sample")

  nCPR <-  zooCountCpr %>%
    dplyr::filter(.data$Copepod == 'COPEPOD') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$Sample, .data$TaxonName) %>%
    unique() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(NoCopepodSpecies_Sample = dplyr::n(), .groups = "drop")

  ShannonCopepodDiversityCPR <- zooCountCpr %>%
    dplyr::filter(.data$Copepod == 'COPEPOD') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(ZCount = sum(.data$TaxonCount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$ZCount, names_from = .data$TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$Sample) %>%
    vegan::diversity('shannon')

  CopepodEvennessCPR <- nCPR %>%
    dplyr::bind_cols(ShannonCopepodDiversityCPR = ShannonCopepodDiversityCPR)  %>%
    dplyr::mutate(CopepodEvenness = .data$ShannonCopepodDiversityCPR / log(.data$NoCopepodSpecies_Sample))

  cprPsamp <- pr_get_CPRSamps("P")

  cprPdat <- pr_get_CPRPhytoData("All")

  # Total Phyto abundance
  phytodatacpr <- cprPsamp %>%
    dplyr::left_join(cprPdat, by = "Sample") %>%
    dplyr::filter(.data$TaxonGroup != 'Other')

  PhytoCcpr <- phytodatacpr %>%
    dplyr::select(.data$Sample, .data$TaxonGroup, .data$PhytoAbund_m3, .data$BioVolume_um3m3) %>%
    pr_add_Carbon("CPR") %>% # Add carbon concentration
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(PhytoBiomassCarbon_pgm3 = sum(.data$Carbon_m3), .groups = "drop")

  TPhytoCpr <- phytodatacpr %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(PhytoAbund_Cellsm3 = sum(.data$PhytoAbund_m3, na.rm = TRUE))

  DDratcpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup %in% c('Centric diatom', "Pennate diatom", 'Dinoflagellate')) %>%
    dplyr::mutate(TaxonGroup = dplyr::recode(.data$TaxonGroup, 'Centric diatom' = 'Diatom', 'Pennate diatom' = 'Diatom')) %>%
    dplyr::select(.data$Sample, .data$TaxonGroup, .data$PhytoAbund_m3) %>%
    dplyr::group_by(.data$Sample, .data$TaxonGroup) %>%
    dplyr::summarise(sumTG = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumTG, names_from = .data$TaxonGroup) %>%
    dplyr::mutate(DiatomDinoflagellateRatio = .data$Diatom / (.data$Diatom + .data$Dinoflagellate))

  AvgCellVolcpr <- phytodatacpr %>%
    dplyr::filter(!is.na(.data$BioVolume_um3m3)) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(AvgCellVol_um3 = mean(sum(.data$BioVolume_um3m3)/sum(.data$PhytoAbund_m3)), .groups = "drop")

  # Diversity (phyto, diatoms, dinos)
  # stick to abundance data here as otherwise we have FOV counts

  npcpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup != 'Other') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$Sample, .data$TaxonName) %>%
    unique() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(NoPhytoSpecies_Sample = dplyr::n(), .groups = "drop")

  ShannonPhytoDiversitycpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup != 'Other') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(Pdata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Pdata, names_from = .data$TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$Sample) %>%
    vegan::diversity('shannon')

  PhytoEvencpr <- npcpr %>%
    dplyr::bind_cols(ShannonPhytoDiversitycpr = ShannonPhytoDiversitycpr) %>%
    dplyr::mutate(PhytoEvenness = .data$ShannonPhytoDiversitycpr / log(.data$NoPhytoSpecies_Sample))

  ndiacpr <-  phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup %in% c('Centric diatom', 'Pennate diatom')) %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$Sample, .data$TaxonName) %>%
    unique() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(NoDiatomSpecies_Sample = dplyr::n())

  ShannonDiatomDiversitycpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup %in% c('Centric diatom', 'Pennate diatom')) %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(Diadata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Diadata, names_from = .data$TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$Sample) %>%
    vegan::diversity('shannon')

  DiaEvencpr <- ndiacpr %>%
    dplyr::bind_cols(ShannonDiatomDiversitycpr = ShannonDiatomDiversitycpr) %>%
    dplyr::mutate(DiatomEvenness = .data$ShannonDiatomDiversitycpr / log(.data$NoDiatomSpecies_Sample))

  ndinocpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup == 'Dinoflagellate') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::select(.data$Sample, .data$TaxonName) %>%
    unique() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(NoDinoSpecies_Sample = dplyr::n())

  ShannonDinoDiversitycpr <- phytodatacpr %>%
    dplyr::filter(.data$TaxonGroup  == 'Dinoflagellate') %>%
    pr_filter_species() %>%
    dplyr::mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    dplyr::group_by(.data$Sample, .data$TaxonName) %>%
    dplyr::summarise(Dinodata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Dinodata, names_from = .data$TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$Sample) %>%
    vegan::diversity('shannon')

  DinoEvencpr <- ndinocpr %>%
    dplyr::bind_cols(ShannonDinoDiversitycpr = ShannonDinoDiversitycpr) %>%
    dplyr::mutate(DinoflagellateEvenness = .data$ShannonDinoDiversitycpr / log(.data$NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  indices <- cprSampleInfo  %>%
    dplyr::left_join(TZoocpr, by = "Sample") %>%
    dplyr::left_join(TCopecpr, by = "Sample") %>%
    dplyr::left_join(ACopeSizeCpr, by = "Sample") %>%
    dplyr::left_join(HCratCpr %>% dplyr::select(-c(.data$Omnivore, .data$Carnivore)), by = "Sample") %>%
    dplyr::left_join(CopepodEvennessCPR,  by = "Sample") %>%
    dplyr::left_join(PhytoCcpr, by = "Sample") %>%
    dplyr::left_join(TPhytoCpr, by = "Sample") %>%
    dplyr::left_join(DDratcpr %>% dplyr::select(-c('Diatom', 'Dinoflagellate')), by = "Sample") %>%
    dplyr::left_join(AvgCellVolcpr, by = "Sample") %>%
    dplyr::left_join(PhytoEvencpr, by = "Sample") %>%
    dplyr::left_join(DiaEvencpr, by = "Sample") %>%
    dplyr::left_join(DinoEvencpr, by = "Sample") %>%
    dplyr::select(-.data$Sample) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), ""))

}

