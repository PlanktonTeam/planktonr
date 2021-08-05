## IMOS plankton data products Indices
## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)

## Created: Sept 2020
## Updated: 11 Nov 2020


#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- create_indices_cpr()
#'
#' @importFrom magrittr "%>%"
#'
create_indices_cpr <- function(){

  get_sat_data <- FALSE

  # Add the bioregions to the CPR data
  cprSampleInfo <- getCPRSamps() %>%
    add_bioregions()

  # ggplot2::ggplot() +
  #   ggplot2::geom_point(data = cprSampleInfo, ggplot2::aes(x = Longitude, y = Latitude), colour = "black") +
  #   ggplot2::geom_point(data = cprSampleInfo, ggplot2::aes(x = Longitude, y = Latitude, colour = BioRegion), size = 0.5)

  cprProps <- readr::read_csv(paste0(get_raw_plankton(), "CPR_SatData.csv"), na = "(null)") %>%
    dplyr::rename(Sample = SAMPLE, ChlorophyllSatellite_mgm3 = CHLA, WaterDepth_m = DEPTH_M)

  cprZsamp <- getCPRSamps() %>%
    dplyr::filter(grepl("Z", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType))

  cprPsamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(SampleType, Biomass_mgm3))

  cprZdat <- getCPRZooData()

  # Total zoop abundance
  zoodatacpr <-  cprZsamp %>%
    dplyr::left_join(cprZdat, by = "Sample")

  TZoocpr <-  zoodatacpr %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(ZoopAbundance_m3 = sum(ZAbund_m3, na.rm = TRUE))

  TCopecpr <- zoodatacpr %>%
    dplyr::filter(Copepod == 'COPEPOD') %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(CopeAbundance_m3 = sum(ZAbund_m3, na.rm = TRUE))

  # Bring in copepod information table with sizes etc.
  Zinfo <- getZooInfo()

  ACopeSizeCpr <- zoodatacpr %>%
    dplyr::filter(Copepod == 'COPEPOD') %>%
    dplyr::inner_join(Zinfo %>% dplyr::select(LENGTH_MM, TaxonName, DIET), by = "TaxonName") %>%
    dplyr::mutate(abunSize = LENGTH_MM * ZAbund_m3) %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(AvgTotalLengthCopepod_mm = sum(abunSize, na.rm = TRUE)/sum(ZAbund_m3, na.rm = TRUE))

  HCratCpr <- zoodatacpr %>%
    dplyr::filter(Copepod == 'COPEPOD') %>%
    dplyr::inner_join(Zinfo %>% dplyr::select(TaxonName, DIET), by = "TaxonName") %>%
    dplyr::mutate(DIET = ifelse(DIET == 'Herbivore', 'Omnivore', DIET)) %>%
    tidyr::drop_na() %>%
    dplyr::select(Sample, DIET, ZAbund_m3) %>%
    dplyr::group_by(Sample, DIET) %>%
    dplyr::summarise(sumdiet = sum(ZAbund_m3 , na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = sumdiet, names_from = DIET) %>%
    dplyr::mutate(HerbivoreCarnivoreCopepodRatio = Carnivore / (Omnivore + Carnivore)) %>% untibble

  # Diversity, evenness etc.

  # Bring in plankton data
  CPRZcount <- getCPRZooCountData()

  zooCountCpr <- cprZsamp %>% # Changed this from cprtr
    dplyr::left_join(CPRZcount, by = "Sample")

  nCPR <-  zooCountCpr %>%
    dplyr::filter(Copepod == 'COPEPOD' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample) %>%
    dplyr::summarise(NoCopepodSpecies_Sample = dplyr::n())

  ShannonCopepodDiversityCPR <- zooCountCpr %>%
    dplyr::filter(Copepod == 'COPEPOD' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(ZCount = sum(TaxonCount, na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = ZCount, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Sample) %>%
    vegan::diversity('shannon')

  CopepodEvennessCPR <- nCPR %>%
    dplyr::bind_cols(ShannonCopepodDiversityCPR = ShannonCopepodDiversityCPR)  %>%
    dplyr::mutate(CopepodEvenness = ShannonCopepodDiversityCPR / log(NoCopepodSpecies_Sample))

  cprPsamp <- getCPRSamps() %>%
    dplyr::filter(grepl("P", SampleType)) %>%
    dplyr::select(-c(PCI, SampleType, Biomass_mgm3))

  cprPdat <- getCPRPhytoData()

  # Total Phyto abundance
  phytodatacpr <- cprPsamp %>%
    dplyr::left_join(cprPdat, by = "Sample") %>%
    dplyr::filter(TaxonGroup != 'Other')

  PhytoCcpr <- phytodatacpr %>%
    dplyr::select(Sample, TaxonGroup, PAbun_m3, BioVolume_um3m3) %>%
    dplyr::mutate(BV_Cell = BioVolume_um3m3 / PAbun_m3, # biovolume of one cell
           Carbon = ifelse(TaxonGroup == 'Dinoflagellate', 0.76*(BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                           ifelse(TaxonGroup == 'Ciliate', 0.22*(BV_Cell)^0.939,
                                  ifelse(TaxonGroup == 'Cyanobacteria', 0.2, 0.288*(BV_Cell)^0.811 ))),
           Carbon_m3 = PAbun_m3 * Carbon) %>% # Carbon per m3
    dplyr::group_by(Sample) %>%
    dplyr::summarise(PhytoBiomassCarbon_pgm3 = sum(Carbon_m3))

  TPhytoCpr <- phytodatacpr %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(AbundancePhyto_cells_m3 = sum(PAbun_m3, na.rm = TRUE))

  DDratcpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup %in% c('Centric diatom', "Pennate diatom", 'Dinoflagellate')) %>%
    dplyr::mutate(TaxonGroup = dplyr::recode(TaxonGroup, 'Centric diatom' = 'Diatom', 'Pennate diatom' = 'Diatom')) %>%
    dplyr::select(Sample, TaxonGroup, PAbun_m3) %>%
    dplyr::group_by(Sample, TaxonGroup) %>%
    dplyr::summarise(sumTG = sum(PAbun_m3, na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = sumTG, names_from = TaxonGroup) %>%
    dplyr::mutate(DiatomDinoflagellateRatio = Diatom / (Diatom + Dinoflagellate)) %>%
    untibble()

  AvgCellVolcpr <- phytodatacpr %>%
    dplyr::filter(!is.na(BioVolume_um3m3)) %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(AvgCellVol_um3 = mean(sum(BioVolume_um3m3)/sum(PAbun_m3)))

  # Diversity (phyto, diatoms, dinos)
  # stick to abundance data here as otherwise we have FOV counts

  npcpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup != 'Other' &
                    Species != "spp." &
                    !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample) %>%
    dplyr::summarise(NoPhytoSpecies_Sample = dplyr::n())

  ShannonPhytoDiversitycpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup != 'Other' &
                    Species != "spp." &
                    !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(Pdata = sum(PAbun_m3, na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = Pdata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Sample) %>%
    vegan::diversity('shannon')

  PhytoEvencpr <- npcpr %>%
    dplyr::bind_cols(ShannonPhytoDiversitycpr = ShannonPhytoDiversitycpr) %>%
    dplyr::mutate(PhytoEvenness = ShannonPhytoDiversitycpr / log(NoPhytoSpecies_Sample))

  ndiacpr <-  phytodatacpr %>%
    dplyr::filter(TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    Species != "spp." &
                    !is.na(Species) & !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample) %>% dplyr::summarise(NoDiatomSpecies_Sample = dplyr::n())

  ShannonDiatomDiversitycpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup %in% c('Centric diatom', 'Pennate diatom') &
                    Species != "spp." & !is.na(Species) &
                    !grepl("cf.", Species) &
                    !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(Diadata = sum(PAbun_m3, na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = Diadata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Sample) %>%
    vegan::diversity('shannon')

  DiaEvencpr <- ndiacpr %>%
    dplyr::bind_cols(ShannonDiatomDiversitycpr = ShannonDiatomDiversitycpr) %>%
    dplyr::mutate(DiatomEvenness = ShannonDiatomDiversitycpr / log(NoDiatomSpecies_Sample))

  ndinocpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup == 'Dinoflagellate' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample) %>% dplyr::summarise(NoDinoSpecies_Sample = dplyr::n())

  ShannonDinoDiversitycpr <- phytodatacpr %>%
    dplyr::filter(TaxonGroup  == 'Dinoflagellate' & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(Sample, TaxonName) %>%
    dplyr::summarise(Dinodata = sum(PAbun_m3, na.rm = TRUE)) %>%
    tidyr::pivot_wider(values_from = Dinodata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Sample) %>%
    vegan::diversity('shannon')

  DinoEvencpr <- ndinocpr %>%
    dplyr::bind_cols(ShannonDinoDiversitycpr = ShannonDinoDiversitycpr) %>%
    dplyr::mutate(DinoflagellateEvenness = ShannonDinoDiversitycpr / log(NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  indices <- cprSampleInfo  %>%
    dplyr::left_join(TZoocpr, by = ("Sample")) %>%
    dplyr::left_join(TCopecpr, by = ("Sample")) %>%
    dplyr::left_join(ACopeSizeCpr, by = ("Sample")) %>%
    dplyr::left_join(HCratCpr %>% dplyr::select(-c(Omnivore, Carnivore)), by = ("Sample")) %>%
    dplyr::left_join(CopepodEvennessCPR,  by = ("Sample")) %>%
    dplyr::left_join(PhytoCcpr, by = ("Sample")) %>%
    dplyr::left_join(TPhytoCpr, by = ("Sample")) %>%
    dplyr::left_join(DDratcpr %>% dplyr::select(-c('Diatom', 'Dinoflagellate')), by = ("Sample")) %>%
    dplyr::left_join(AvgCellVolcpr, by = ("Sample")) %>%
    dplyr::left_join(PhytoEvencpr, by = ("Sample")) %>%
    dplyr::left_join(DiaEvencpr, by = ("Sample")) %>%
    dplyr::left_join(DinoEvencpr, by = ("Sample")) %>%
    #  dplyr::left_join(satcpr %>% dplyr::select(Sample, sst_1d, chl_oc3_1d), by = ("Sample")) %>%  #add once run , GSLA, GSL, UCUR, VCUR
    dplyr::select(-Sample, -SampleType)

  # # make indices table (nrows must always equal nrows of Trips) - old one for IMOS
  # indices <-  cprTrips  %>%
  #   dplyr::left_join(cprProps, by = ("Sample")) %>%
  #   dplyr::left_join(TZoocpr, by = ("Sample")) %>%
  #   dplyr::left_join(TCopecpr, by = ("Sample")) %>%
  #   dplyr::left_join(ACopeSizeCpr, by = ("Sample")) %>%
  #   dplyr::left_join(HCratCpr %>% dplyr::select(-c('CO', 'CC')), by = ("Sample")) %>%
  #   dplyr::left_join(CopepodEvennessCPR,  by = ("Sample")) %>%
  #   dplyr::left_join(PhytoCcpr, by = ("Sample")) %>%
  #   dplyr::left_join(TPhytoCpr, by = ("Sample")) %>%
  #   dplyr::left_join(DDratcpr %>% dplyr::select(-c('Diatom', 'Dinoflagellate')), by = ("Sample")) %>%
  #   dplyr::left_join(AvgCellVolcpr, by = ("Sample")) %>%
  #   dplyr::left_join(PhytoEvencpr, by = ("Sample")) %>%
  #   dplyr::left_join(DiaEvencpr, by = ("Sample")) %>%
  #   dplyr::left_join(DinoEvencpr, by = ("Sample")) %>%
  #   dplyr::select(-Sample)

  return(indices)

  # fwrite(IndicesCPR, file = paste0("Output/",.Platform$file.sep, "CPR_Indices.csv"), row.names = FALSE)

  # test table
  # n should be 1, replicates or duplicate samples will have values > 1
  # test <- IndicesCPR %>%
  # dplyr::group_by(Latitude, Longitude, SampleDateUTC) %>%
  # dplyr::summarise(n = dplyr::n())

  # max(test$n)

}
