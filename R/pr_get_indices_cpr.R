#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- pr_get_indices_cpr()
#'

#'
pr_get_indices_cpr <- function(){

  # Add the bioregions to the CPR data
  cprSampleInfo <- pr_get_CPRSamps() %>%
    pr_add_bioregions()

  cprZsamp <- pr_get_CPRSamps() %>%
    filter(grepl("Z", SampleType)) %>%
    select(-c(PCI, SampleType))

  cprPsamp <- pr_get_CPRSamps() %>%
    filter(grepl("P", SampleType)) %>%
    select(-c(SampleType, Biomass_mgm3))

  cprZdat <- pr_get_CPRZooData()

  # Total zoop abundance
  zoodatacpr <-  cprZsamp %>%
    left_join(cprZdat, by = "Sample")

  TZoocpr <-  zoodatacpr %>%
    group_by(Sample) %>%
    summarise(ZoopAbundance_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  TCopecpr <- zoodatacpr %>%
    filter(Copepod == 'COPEPOD') %>%
    group_by(Sample) %>%
    summarise(CopeAbundance_m3 = sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  # Bring in copepod information table with sizes etc.
  Zinfo <- pr_get_ZooInfo()

  ACopeSizeCpr <- zoodatacpr %>%
    filter(Copepod == 'COPEPOD') %>%
    inner_join(Zinfo %>% select(LENGTH_MM, TaxonName, DIET), by = "TaxonName") %>%
    mutate(abunSize = LENGTH_MM * ZooPhytoAbund_m3) %>%
    group_by(Sample) %>%
    summarise(AvgTotalLengthCopepod_mm = sum(abunSize, na.rm = TRUE)/sum(ZooPhytoAbund_m3, na.rm = TRUE), .groups = "drop")

  HCratCpr <- zoodatacpr %>%
    filter(Copepod == 'COPEPOD') %>%
    inner_join(Zinfo %>% select(TaxonName, DIET), by = "TaxonName") %>%
    mutate(DIET = ifelse(DIET == 'Herbivore', 'Omnivore', DIET)) %>%
    tidyr::drop_na() %>%
    select(Sample, DIET, ZooPhytoAbund_m3) %>%
    group_by(Sample, DIET) %>%
    summarise(sumdiet = sum(ZooPhytoAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumdiet, names_from = DIET) %>%
    mutate(HerbivoreCarnivoreCopepodRatio = Carnivore / (Omnivore + Carnivore))

  # Diversity, evenness etc.

  # Bring in plankton data
  CPRZcount <- pr_get_CPRZooCountData()

  zooCountCpr <- cprZsamp %>% # Changed this from cprtr
    left_join(CPRZcount, by = "Sample")

  nCPR <-  zooCountCpr %>%
    filter(Copepod == 'COPEPOD' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample) %>%
    summarise(NoCopepodSpecies_Sample = n(), .groups = "drop")

  ShannonCopepodDiversityCPR <- zooCountCpr %>%
    filter(Copepod == 'COPEPOD' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample, TaxonName) %>%
    summarise(ZCount = sum(TaxonCount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = ZCount, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-Sample) %>%
    vegan::diversity('shannon')

  CopepodEvennessCPR <- nCPR %>%
    bind_cols(ShannonCopepodDiversityCPR = ShannonCopepodDiversityCPR)  %>%
    mutate(CopepodEvenness = ShannonCopepodDiversityCPR / log(NoCopepodSpecies_Sample))

  cprPsamp <- pr_get_CPRSamps() %>%
    filter(grepl("P", SampleType)) %>%
    select(-c(PCI, SampleType, Biomass_mgm3))

  cprPdat <- pr_get_CPRPhytoData()

  # Total Phyto abundance
  phytodatacpr <- cprPsamp %>%
    left_join(cprPdat, by = "Sample") %>%
    filter(TaxonGroup != 'Other')

  PhytoCcpr <- phytodatacpr %>%
    select(Sample, TaxonGroup, PhytoAbund_m3, BioVolume_um3m3) %>%
    mutate(BV_Cell = BioVolume_um3m3 / PhytoAbund_m3, # biovolume of one cell
           Carbon = ifelse(TaxonGroup == 'Dinoflagellate', 0.76*(BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                           ifelse(TaxonGroup == 'Ciliate', 0.22*(BV_Cell)^0.939,
                                  ifelse(TaxonGroup == 'Cyanobacteria', 0.2, 0.288*(BV_Cell)^0.811 ))),
           Carbon_m3 = PhytoAbund_m3 * Carbon) %>% # Carbon per m3
    group_by(Sample) %>%
    summarise(PhytoBiomassCarbon_pgm3 = sum(Carbon_m3), .groups = "drop")

  TPhytoCpr <- phytodatacpr %>%
    group_by(Sample) %>%
    summarise(AbundancePhyto_cells_m3 = sum(PhytoAbund_m3, na.rm = TRUE))

  DDratcpr <- phytodatacpr %>%
    filter(TaxonGroup %in% c('Centric diatom', "Pennate diatom", 'Dinoflagellate')) %>%
    mutate(TaxonGroup = recode(TaxonGroup, 'Centric diatom' = 'Diatom', 'Pennate diatom' = 'Diatom')) %>%
    select(Sample, TaxonGroup, PhytoAbund_m3) %>%
    group_by(Sample, TaxonGroup) %>%
    summarise(sumTG = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumTG, names_from = TaxonGroup) %>%
    mutate(DiatomDinoflagellateRatio = Diatom / (Diatom + Dinoflagellate))

  AvgCellVolcpr <- phytodatacpr %>%
    filter(!is.na(BioVolume_um3m3)) %>%
    group_by(Sample) %>%
    summarise(AvgCellVol_um3 = mean(sum(BioVolume_um3m3)/sum(PhytoAbund_m3)), .groups = "drop")

  # Diversity (phyto, diatoms, dinos)
  # stick to abundance data here as otherwise we have FOV counts

  npcpr <- phytodatacpr %>%
    filter(TaxonGroup != 'Other' &
                    Species != "spp." &
                    !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample) %>%
    summarise(NoPhytoSpecies_Sample = n(), .groups = "drop")

  ShannonPhytoDiversitycpr <- phytodatacpr %>%
    filter(TaxonGroup != 'Other' &
                    Species != "spp." &
                    !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample, TaxonName) %>%
    summarise(Pdata = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Pdata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-Sample) %>%
    vegan::diversity('shannon')

  PhytoEvencpr <- npcpr %>%
    bind_cols(ShannonPhytoDiversitycpr = ShannonPhytoDiversitycpr) %>%
    mutate(PhytoEvenness = ShannonPhytoDiversitycpr / log(NoPhytoSpecies_Sample))

  ndiacpr <-  phytodatacpr %>%
    filter(TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    Species != "spp." &
                    !is.na(Species) & !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample) %>% summarise(NoDiatomSpecies_Sample = n())

  ShannonDiatomDiversitycpr <- phytodatacpr %>%
    filter(TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    Species != "spp." & !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample, TaxonName) %>%
    summarise(Diadata = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Diadata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-Sample) %>%
    vegan::diversity('shannon')

  DiaEvencpr <- ndiacpr %>%
    bind_cols(ShannonDiatomDiversitycpr = ShannonDiatomDiversitycpr) %>%
    mutate(DiatomEvenness = ShannonDiatomDiversitycpr / log(NoDiatomSpecies_Sample))

  ndinocpr <- phytodatacpr %>%
    filter(TaxonGroup == 'Dinoflagellate' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample) %>% summarise(NoDinoSpecies_Sample = n())

  ShannonDinoDiversitycpr <- phytodatacpr %>%
    filter(TaxonGroup  == 'Dinoflagellate' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    group_by(Sample, TaxonName) %>%
    summarise(Dinodata = sum(PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Dinodata, names_from = TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-Sample) %>%
    vegan::diversity('shannon')

  DinoEvencpr <- ndinocpr %>%
    bind_cols(ShannonDinoDiversitycpr = ShannonDinoDiversitycpr) %>%
    mutate(DinoflagellateEvenness = ShannonDinoDiversitycpr / log(NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  indices <- cprSampleInfo  %>%
    left_join(TZoocpr, by = ("Sample")) %>%
    left_join(TCopecpr, by = ("Sample")) %>%
    left_join(ACopeSizeCpr, by = ("Sample")) %>%
    left_join(HCratCpr %>% select(-c(Omnivore, Carnivore)), by = ("Sample")) %>%
    left_join(CopepodEvennessCPR,  by = ("Sample")) %>%
    left_join(PhytoCcpr, by = ("Sample")) %>%
    left_join(TPhytoCpr, by = ("Sample")) %>%
    left_join(DDratcpr %>% select(-c('Diatom', 'Dinoflagellate')), by = ("Sample")) %>%
    left_join(AvgCellVolcpr, by = ("Sample")) %>%
    left_join(PhytoEvencpr, by = ("Sample")) %>%
    left_join(DiaEvencpr, by = ("Sample")) %>%
    left_join(DinoEvencpr, by = ("Sample")) %>%
    select(-Sample, -SampleType)

  return(indices)
}
