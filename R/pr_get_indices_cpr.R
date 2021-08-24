#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- pr_get_indices_cpr()
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_indices_cpr <- function(){

  # Add the bioregions to the CPR data
  cprSampleInfo <- pr_get_CPRSamps() %>%
    pr_add_bioregions()

  cprZsamp <- pr_get_CPRSamps(c("Z", "B"))

  cprZdat <- pr_get_CPRZooData()

  # Total zoop abundance
  zoodatacpr <- cprZsamp %>%
    left_join(cprZdat, by = "Sample")

  TZoocpr <- zoodatacpr %>%
    group_by(.data$Sample) %>%
    summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  TCopecpr <- zoodatacpr %>%
    filter(.data$Copepod == 'COPEPOD') %>%
    group_by(.data$Sample) %>%
    summarise(CopeAbundance_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  # Bring in copepod information table with sizes etc.
  Zinfo <- pr_get_ZooInfo()

  ACopeSizeCpr <- zoodatacpr %>%
    filter(.data$Copepod == 'COPEPOD') %>%
    inner_join(Zinfo %>% select(.data$Length_mm, .data$TaxonName, .data$Diet), by = "TaxonName") %>%
    mutate(abunSize = .data$Length_mm * .data$ZoopAbund_m3) %>%
    group_by(.data$Sample) %>%
    summarise(AvgTotalLengthCopepod_mm = sum(.data$abunSize, na.rm = TRUE)/sum(.data$ZoopAbund_m3, na.rm = TRUE), .groups = "drop")

  HCratCpr <- zoodatacpr %>%
    filter(.data$Copepod == 'COPEPOD') %>%
    inner_join(Zinfo %>% select(.data$TaxonName, .data$Diet), by = "TaxonName") %>%
    mutate(Diet = ifelse(.data$Diet == 'Herbivore', 'Omnivore', .data$Diet)) %>%
    tidyr::drop_na() %>%
    select(.data$Sample, .data$Diet, .data$ZoopAbund_m3) %>%
    group_by(.data$Sample, .data$Diet) %>%
    summarise(sumdiet = sum(.data$ZoopAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumdiet, names_from = .data$Diet) %>%
    mutate(HerbivoreCarnivoreCopepodRatio = .data$Carnivore / (.data$Omnivore + .data$Carnivore))

  # Diversity, evenness etc.

  # Bring in plankton data
  CPRZcount <- pr_get_CPRZooData("Count")

  zooCountCpr <- cprZsamp %>% # Changed this from cprtr
    left_join(CPRZcount, by = "Sample")

  nCPR <-  zooCountCpr %>%
    filter(.data$Copepod == 'COPEPOD' & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample) %>%
    summarise(NoCopepodSpecies_Sample = n(), .groups = "drop")

  ShannonCopepodDiversityCPR <- zooCountCpr %>%
    filter(.data$Copepod == 'COPEPOD' & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample, .data$TaxonName) %>%
    summarise(ZCount = sum(.data$TaxonCount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$ZCount, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$Sample) %>%
    vegan::diversity('shannon')

  CopepodEvennessCPR <- nCPR %>%
    bind_cols(ShannonCopepodDiversityCPR = ShannonCopepodDiversityCPR)  %>%
    mutate(CopepodEvenness = .data$ShannonCopepodDiversityCPR / log(.data$NoCopepodSpecies_Sample))

  cprPsamp <- pr_get_CPRSamps("P")

  cprPdat <- pr_get_CPRPhytoData("All")

  # Total Phyto abundance
  phytodatacpr <- cprPsamp %>%
    left_join(cprPdat, by = "Sample") %>%
    filter(.data$TaxonGroup != 'Other')

  PhytoCcpr <- phytodatacpr %>%
    select(.data$Sample, .data$TaxonGroup, .data$PhytoAbund_m3, .data$BioVolume_um3m3) %>%
    mutate(BV_Cell = .data$BioVolume_um3m3 / .data$PhytoAbund_m3, # biovolume of one cell
           Carbon = ifelse(.data$TaxonGroup == "Dinoflagellate", 0.76*(.data$BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                           ifelse(.data$TaxonGroup == 'Ciliate', 0.22*(.data$BV_Cell)^0.939,
                                  ifelse(.data$TaxonGroup == 'Cyanobacteria', 0.2, 0.288*(.data$BV_Cell)^0.811 ))),
           Carbon_m3 = .data$PhytoAbund_m3 * .data$Carbon) %>% # Carbon per m3
    group_by(.data$Sample) %>%
    summarise(PhytoBiomassCarbon_pgm3 = sum(.data$Carbon_m3), .groups = "drop")

  TPhytoCpr <- phytodatacpr %>%
    group_by(.data$Sample) %>%
    summarise(AbundancePhyto_cells_m3 = sum(.data$PhytoAbund_m3, na.rm = TRUE))

  DDratcpr <- phytodatacpr %>%
    filter(.data$TaxonGroup %in% c('Centric diatom', "Pennate diatom", 'Dinoflagellate')) %>%
    mutate(TaxonGroup = recode(.data$TaxonGroup, 'Centric diatom' = 'Diatom', 'Pennate diatom' = 'Diatom')) %>%
    select(.data$Sample, .data$TaxonGroup, .data$PhytoAbund_m3) %>%
    group_by(.data$Sample, .data$TaxonGroup) %>%
    summarise(sumTG = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$sumTG, names_from = .data$TaxonGroup) %>%
    mutate(DiatomDinoflagellateRatio = .data$Diatom / (.data$Diatom + .data$Dinoflagellate))

  AvgCellVolcpr <- phytodatacpr %>%
    filter(!is.na(.data$BioVolume_um3m3)) %>%
    group_by(.data$Sample) %>%
    summarise(AvgCellVol_um3 = mean(sum(.data$BioVolume_um3m3)/sum(.data$PhytoAbund_m3)), .groups = "drop")

  # Diversity (phyto, diatoms, dinos)
  # stick to abundance data here as otherwise we have FOV counts

  npcpr <- phytodatacpr %>%
    filter(.data$TaxonGroup != 'Other' &
                    .data$Species != "spp." &
                    !is.na(.data$Species) &
                    !grepl("cf.", .data$Species) &
                    !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample) %>%
    summarise(NoPhytoSpecies_Sample = n(), .groups = "drop")

  ShannonPhytoDiversitycpr <- phytodatacpr %>%
    filter(.data$TaxonGroup != 'Other' &
             .data$Species != "spp." &
                    !is.na(.data$Species) &
                    !grepl("cf.", .data$Species) &
                    !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample, .data$TaxonName) %>%
    summarise(Pdata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Pdata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$Sample) %>%
    vegan::diversity('shannon')

  PhytoEvencpr <- npcpr %>%
    bind_cols(ShannonPhytoDiversitycpr = ShannonPhytoDiversitycpr) %>%
    mutate(PhytoEvenness = .data$ShannonPhytoDiversitycpr / log(.data$NoPhytoSpecies_Sample))

  ndiacpr <-  phytodatacpr %>%
    filter(.data$TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    .data$Species != "spp." &
                    !is.na(.data$Species) & !grepl("cf.", .data$Species) &
                    !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample) %>%
    summarise(NoDiatomSpecies_Sample = n())

  ShannonDiatomDiversitycpr <- phytodatacpr %>%
    filter(.data$TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    .data$Species != "spp." & !is.na(.data$Species) &
                    !grepl("cf.", .data$Species) &
                    !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample, .data$TaxonName) %>%
    summarise(Diadata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Diadata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$Sample) %>%
    vegan::diversity('shannon')

  DiaEvencpr <- ndiacpr %>%
    bind_cols(ShannonDiatomDiversitycpr = ShannonDiatomDiversitycpr) %>%
    mutate(DiatomEvenness = .data$ShannonDiatomDiversitycpr / log(.data$NoDiatomSpecies_Sample))

  ndinocpr <- phytodatacpr %>%
    filter(.data$TaxonGroup == 'Dinoflagellate' & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample) %>%
    summarise(NoDinoSpecies_Sample = n())

  ShannonDinoDiversitycpr <- phytodatacpr %>%
    filter(.data$TaxonGroup  == 'Dinoflagellate' & .data$Species != "spp." & !is.na(.data$Species) & !grepl("cf.", .data$Species) & !grepl("grp", .data$Species)) %>%
    mutate(TaxonName = paste0(.data$Genus," ", stringr::word(.data$Species,1))) %>% # bin complexes
    group_by(.data$Sample, .data$TaxonName) %>%
    summarise(Dinodata = sum(.data$PhytoAbund_m3, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = .data$Dinodata, names_from = .data$TaxonName, values_fill = 0) %>%
    ungroup() %>%
    select(-.data$Sample) %>%
    vegan::diversity('shannon')

  DinoEvencpr <- ndinocpr %>%
    bind_cols(ShannonDinoDiversitycpr = ShannonDinoDiversitycpr) %>%
    mutate(DinoflagellateEvenness = .data$ShannonDinoDiversitycpr / log(.data$NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  indices <- cprSampleInfo  %>%
    left_join(TZoocpr, by = "Sample") %>%
    left_join(TCopecpr, by = "Sample") %>%
    left_join(ACopeSizeCpr, by = "Sample") %>%
    left_join(HCratCpr %>% select(-c(.data$Omnivore, .data$Carnivore)), by = "Sample") %>%
    left_join(CopepodEvennessCPR,  by = "Sample") %>%
    left_join(PhytoCcpr, by = "Sample") %>%
    left_join(TPhytoCpr, by = "Sample") %>%
    left_join(DDratcpr %>% select(-c('Diatom', 'Dinoflagellate')), by = "Sample") %>%
    left_join(AvgCellVolcpr, by = "Sample") %>%
    left_join(PhytoEvencpr, by = "Sample") %>%
    left_join(DiaEvencpr, by = "Sample") %>%
    left_join(DinoEvencpr, by = "Sample") %>%
    select(-.data$Sample)

}
