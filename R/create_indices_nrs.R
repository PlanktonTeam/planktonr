## IMOS plankton data products Indices
## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)

## Created: Sept 2020
## Updated:

#' Create a range of indices
#'
#' @return A dataframe with with NRS indices
#' @export
#'
#' @examples
#' df <- create_indices_nrs()
#' @importFrom magrittr "%>%"
create_indices_nrs <- function(){
  # source("../Satellite/fIMOS_MatchAltimetry.R")
  # source("../Satellite/fIMOS_MatchMODIS.R")
  # source("../Satellite/fIMOS_MatchGHRSST.R")

  # uses mostly the same raw data from IMOS_PlanktonProducts_Create.R
  # note there are circumstances where a trip won"t have a phyto and a zoo samples due to loss of sample etc.

  NRSdat <- getNRSTrips() %>%
    dplyr::select(-SampleType) %>%
    dplyr::filter(Station != "Port Hacking 4") #ignore warning, "fast" method does better here than "accurate"

  dNRSdat <- dplyr::distinct(NRSdat, TripCode, .keep_all = TRUE) %>% # Distinct rows for satellite, should be anyway
    dplyr::rename(Date = SampleDateLocal) %>%
    dplyr::select(TripCode, Date, Latitude, Longitude)

  # SST and Chlorophyll from CTD
  CTD <- getCTD() %>%
    dplyr::filter(Depth_m < 15) %>% # take average of top 10m as a surface value for SST and CHL, this is removing 17 casts as of nov 2020
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(CTD_SST_C = mean(Temperature_degC, na.rm = TRUE),
                     CTDChla_mgm3 = mean(Chla_mgm3, na.rm = TRUE),
                     CTDSalinity_psu = mean(Salinity_psu, na.rm = TRUE),
                     .groups = "drop") %>%
    untibble()

  # data set for calculating MLD
  CTD_MLD <- getCTD() %>%
    dplyr::select(TripCode, Temperature_degC, Chla_mgm3, Salinity_psu, Depth_m) %>%
    dplyr::rename(CTDTemperature = Temperature_degC, CTDSalinity = Salinity_psu, CTDChlF_mgm3 = Chla_mgm3, SampleDepth_m = Depth_m) %>%
    tidyr::drop_na(TripCode)

  MLD <- data.frame(TripCode = character(), MLD_temp = numeric(), MLD_sal = numeric(), DCM = numeric())

  # MLD by T and S (Ref: Condie & Dunn 2006)
  # DCM from max f from CTD
  for (i in 1:length(unique(CTD_MLD$TripCode))) {
    dat <- CTD_MLD %>%
      dplyr::select(TripCode) %>%
      dplyr::distinct() %>%
      dplyr::mutate(TripCode = as.factor(TripCode))

    Trip <- dat$TripCode[[i]] %>%
      droplevels()

    mldData <- CTD_MLD %>%
      dplyr::filter(TripCode == Trip) %>%
      dplyr::arrange(SampleDepth_m)

    if (as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 5
    }

    if (!as.character(substr(Trip, 0,3)) %in% c("DAR", "YON")){
      refDepth <- 10
    }

    # Extract refence depth (z)
    refz <- mldData %>%
      dplyr::mutate(refd = abs(SampleDepth_m - refDepth), # find depth nearest to 10 m
                    rankrefd = stats::ave(refd, FUN = . %>% order %>% order)) %>%
      dplyr::filter(rankrefd == 1)

    # Reference Temperature
    refT <- refz$CTDTemperature - 0.4 # temp at 10 m minus 0.4 deg C

    mldData <- mldData %>%
      dplyr::filter(SampleDepth_m > refz$SampleDepth_m)

    mld_t <- mldData %>%
      dplyr::mutate(temp = abs(CTDTemperature - refT),
                    ranktemp = stats::ave(temp, FUN = . %>% order %>% order)) %>%
      dplyr::filter(ranktemp == 1)
    MLD_temp <- mld_t$SampleDepth_m

    refS <- refz$CTDSalinity - 0.03 # temp at 10 m minus 0.4
    mld_s <- mldData %>%
      dplyr::mutate(temp = abs(CTDSalinity - refS),
                    ranksal = stats::ave(temp, FUN = . %>% order %>% order)) %>%
      dplyr::filter(ranksal == 1)
    MLD_sal <- mld_s$SampleDepth_m

    dcm <- (mldData %>%
              dplyr::filter(CTDChlF_mgm3 > 0 & CTDChlF_mgm3 == max(CTDChlF_mgm3))
    )$SampleDepth_m
    dcm[rlang::is_empty(dcm)] = NA

    MLD <- MLD %>%
      dplyr::bind_rows(data.frame(TripCode = as.character(Trip), MLD_temp = MLD_temp, MLD_sal = MLD_sal, DCM = dcm)) %>%
      tidyr::drop_na(TripCode)
  }

  # # Access satellite data for the sample dates using the IMOS_Toolbox
  #
  # # If on Windows you will need to install a development
  # # version of ncdf4 which allows the use of OpenDAP
  # if(.Platform$OS.type == "windows") {
  #  warning("It looks like you are on a Windows PC - You will need to install a
  #  development version of ncdf4 which allows the use of OpenDAP. Please
  #  run devtools::install_github("mdsumner/ncdf4") to install or
  #  see "https://github.com/mdsumner/ncdf4" for more information.")
  # }
  #
  # # Get GHRSST SST Data
  # # Possible products to download are:
  # # dt_analysis, l2p_flags, quality_level, satellite_zenith_angle, sea_ice_fraction, sea_ice_fraction_dtime_from_sst,
  # # sea_surface_temperature, sea_surface_temperature_day_night, sses_bias, sses_count,sses_standard_deviation,
  # # sst_count, sst_dtime, sst_mean, sst_standard_deviation, wind_speed, wind_speed_dtime_from_sst,
  # res_temp <- "1d"
  # res_spat <- 10 # Return the average of res_spat x res_spat pixels
  # pr <- ("sea_surface_temperature")
  # GHRSST <- fIMOS_MatchGHRSST(dNRSdat, pr, res_temp, res_spat)
  #
  # # Get MODIS Data
  # # Possible products
  # # pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par",
  # #     "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
  # #     "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")
  #
  # pr <- c("chl_oci")
  # res_temp <- "1d"
  # res_spat <- 10 # Return the average of res_spat x res_spat pixels
  # MODIS <- fIMOS_MatchMODIS(dNRSdat, pr, res_temp, res_spat)
  #
  #
  # # Get Altimetry (Gridded sea level anomaly, Gridded sea level, Surface geostrophic velocity)
  # dNRSdat <- dNRSdat[1:3,]
  # Alt <- fIMOS_MatchAltimetry(dNRSdat, res_spat)


  # Nutrient data

  Nuts <- getChemistry() %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(Silicate_umolL = mean(Silicate_umolL, na.rm = TRUE),
                     Phosphate_umolL = mean(Phosphate_umolL, na.rm = TRUE),
                     Ammonium_umolL = mean(Ammonium_umolL, na.rm = TRUE),
                     Nitrate_umolL = mean(Nitrate_umolL, na.rm = TRUE),
                     Nitrite_umolL = mean(Nitrite_umolL, na.rm = TRUE),
                     Oxygen_umolL = mean(Oxygen_umolL, na.rm = TRUE),
                     DIC_umolkg = mean(DIC_umolkg, na.rm = TRUE),
                     TAlkalinity_umolkg = mean(TAlkalinity_umolkg, na.rm = TRUE),
                     Salinity_PSU = mean(Salinity_PSU, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    untibble()

  Pigments <- getPigments() %>%
    dplyr::filter(SampleDepth_m <= 25) %>% # take average of top 10m as a surface value for SST and CHL
    # dplyr::filter(SampleDepth_m == "WC") %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(Chla_mgm3 = mean(DV_CPHL_A_AND_CPHL_A, na.rm = TRUE),
                     .groups = "drop") %>%
    untibble()

  # Total Zooplankton Abundance
  ZooData <- getNRSTrips() %>%
    dplyr::left_join(getNRSZooData(), by = "TripCode")

  TZoo <- ZooData %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(ZoopAbundance_m3 = sum(ZAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  TCope <- ZooData %>%
    dplyr::filter(Copepod == "COPEPOD") %>%
    dplyr::group_by(TripCode, ) %>%
    dplyr::summarise(CopeAbundance_m3 = sum(ZAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  # Bring in copepod information table with sizes etc.
  ZInfo <- getZooInfo()

  ACopeSize <- ZooData %>%
    dplyr::filter(Copepod == "COPEPOD") %>%
    dplyr::inner_join(ZInfo %>%
                        dplyr::select(LENGTH_MM, TaxonName, DIET), by = "TaxonName") %>%
    dplyr::mutate(abunSize = LENGTH_MM * ZAbund_m3,
                  DIET = ifelse(DIET == "CC", "CC", "CO")) %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(AvgTotalLengthCopepod_mm = sum(abunSize, na.rm = TRUE)/sum(ZAbund_m3, na.rm = TRUE),
                     .groups = "drop")

  HCrat <- ZooData %>% #TODO This whole section needs to be reconsidered. Not sure it gives the correct ratios
    dplyr::filter(Copepod == "COPEPOD") %>%
    dplyr::inner_join(ZInfo %>%
                        dplyr::select(TaxonName, DIET), by = "TaxonName") %>%
    dplyr::mutate(DIET = dplyr::case_when(
                  DIET == "Carnivore" ~ "CC",
                  DIET == "Omnivore" ~ "CO",
                  DIET == "Herbivore" ~ "CO")) %>% #TODO Check that Herbivore is correct
    tidyr::drop_na() %>%
    dplyr::select(TripCode, DIET, ZAbund_m3) %>%
    dplyr::group_by(TripCode, DIET) %>%
    dplyr::summarise(sumdiet = sum(ZAbund_m3 , na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumdiet, names_from = DIET) %>%
    dplyr::mutate(HerbivoreCarnivoreCopepodRatio = CO / (CO + CC)) %>%
    untibble()

  # Diversity, evenness etc.

  # Bring in plankton data
  ZooCount <- getNRSTrips() %>%
    dplyr::left_join(getNRSZooCount(), by = "TripCode")

  zoo_n <- ZooCount %>%
    dplyr::filter(Copepod == "COPEPOD" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(NoCopepodSpecies_Sample = dplyr::n(), .groups = "drop")

  ShannonCopepodDiversity <- ZooCount %>%
    dplyr::filter(Copepod == "COPEPOD" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(ZCount = sum(TaxonCount, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = ZCount, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TripCode) %>%
    vegan::diversity("shannon")

  CopepodEvenness <- zoo_n %>%
    dplyr::bind_cols(ShannonCopepodDiversity = ShannonCopepodDiversity) %>%
    dplyr::mutate(CopepodEvenness = ShannonCopepodDiversity / log(NoCopepodSpecies_Sample))

  # Total Phyto abundance
  PhytoData <- getNRSTrips() %>%
    dplyr::left_join(getNRSPhytoData(), by = "TripCode") %>%
    dplyr::filter(TaxonGroup != "Other")

  # PhytoData <- PhytoData %>%
  #  dplyr::filter(str_detect(TaxonName, "Flagellate <10", negate = TRUE)) # Remove flagellates #TODO

  PhytoC <- PhytoData %>%
    dplyr::select(TripCode, TaxonGroup, Cells_L, Biovolume_um3L) %>%
    dplyr::mutate(BV_Cell = Biovolume_um3L / Cells_L, # biovolume of one cell
                  Carbon = ifelse(TaxonGroup == "Dinoflagellate", 0.76*(BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                  ifelse(TaxonGroup == "Ciliate", 0.22*(BV_Cell)^0.939,
                                         ifelse(TaxonGroup == "Cyanobacteria", 0.2, 0.288*(BV_Cell)^0.811 ))),
                  Carbon_L = Cells_L * Carbon) %>% # Carbon per litre
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(PhytoBiomassCarbon_pg_L = sum(Carbon_L),
                     .groups = "drop")

  TPhyto <- PhytoData %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(AbundancePhyto_cells_L = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop")

  DDrat <- PhytoData %>%
    dplyr::filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom", "Dinoflagellate")) %>%
    dplyr::mutate(TaxonGroup = dplyr::recode(TaxonGroup, "Centric diatom" = "Diatom", "Pennate diatom" = "Diatom")) %>%
    dplyr::select(TripCode, TaxonGroup, Cells_L) %>%
    dplyr::group_by(TripCode, TaxonGroup) %>%
    dplyr::summarise(sumTG = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = sumTG, names_from = TaxonGroup) %>%
    dplyr::mutate(DiatomDinoflagellateRatio = Diatom / (Diatom + Dinoflagellate)) %>%
    untibble()

  AvgCellVol <- PhytoData %>%
    dplyr::filter(!is.na(Biovolume_um3L)) %>%
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(AvgCellVol_um3 = mean(sum(Biovolume_um3L)/sum(Cells_L)),
                     .groups = "drop")

  # vegan::diversity (phyto, diatoms, dinos)
  # stick to abundance data here or we lose all the data that Pru counted which we don"t have counts for.

  NP <- PhytoData %>%
    dplyr::filter(TaxonGroup != "Other" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(NoPhytoSpecies_Sample = dplyr::n(),
                     .groups = "drop")

  ShannonPhytoDiversity <- PhytoData %>%
    dplyr::filter(TaxonGroup != "Other" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(Pdata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Pdata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TripCode) %>%
    vegan::diversity("shannon")

  PhytoEven <- NP %>%
    dplyr::bind_cols(ShannonPhytoDiversity = ShannonPhytoDiversity) %>%
    dplyr::mutate(PhytoEvenness = ShannonPhytoDiversity / log(NoPhytoSpecies_Sample))

  NDia <- PhytoData %>%
    dplyr::filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom") & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(NoDiatomSpecies_Sample = dplyr::n(),
                     .groups = "drop")

  ShannonDiatomDiversity <- PhytoData %>%
    dplyr::filter(TaxonGroup %in% c("Centric diatom", "Pennate diatom") & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(Diadata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Diadata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TripCode) %>%
    vegan::diversity("shannon")

  DiaEven <- NDia %>%
    dplyr::bind_cols(ShannonDiatomDiversity = ShannonDiatomDiversity) %>%
    dplyr::mutate(DiatomEvenness = ShannonDiatomDiversity / log(NoDiatomSpecies_Sample))

  NDino <- PhytoData %>%
    dplyr::filter(TaxonGroup == "Dinoflagellate" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode) %>%
    dplyr::summarise(NoDinoSpecies_Sample = dplyr::n(),
                     .groups = "drop")

  ShannonDinoDiversity <- PhytoData %>%
    dplyr::filter(TaxonGroup == "Dinoflagellate" & Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species)) %>%
    dplyr::mutate(TaxonName = paste0(Genus," ", stringr::word(Species,1))) %>% # bin complexes
    dplyr::group_by(TripCode, TaxonName) %>%
    dplyr::summarise(Dinodata = sum(Cells_L, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(values_from = Dinodata, names_from = TaxonName, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TripCode) %>%
    vegan::diversity("shannon")

  DinoEven <- NDino %>%
    dplyr::bind_cols(ShannonDiatomDiversity = ShannonDinoDiversity) %>%
    dplyr::mutate(DinoflagellateEvenness = ShannonDinoDiversity / log(NoDinoSpecies_Sample))

  # make indices table (nrows must always equal nrows of Trips)
  Indices <- NRSdat %>%
    dplyr::left_join(TZoo, by = ("TripCode")) %>%
    dplyr::left_join(TCope, by = ("TripCode")) %>%
    dplyr::left_join(ACopeSize, by = ("TripCode")) %>%
    dplyr::left_join(HCrat %>%
                       dplyr::select(-c("CO", "CC")), ("TripCode")) %>%
    dplyr::left_join(CopepodEvenness, by = ("TripCode")) %>%
    dplyr::left_join(PhytoC, by = ("TripCode")) %>%
    dplyr::left_join(TPhyto, by = ("TripCode")) %>%
    dplyr::left_join(DDrat %>% dplyr::select(-c("Diatom", "Dinoflagellate")), by = ("TripCode")) %>%
    dplyr::left_join(AvgCellVol, by = ("TripCode")) %>%
    dplyr::left_join(PhytoEven, by = ("TripCode")) %>%
    dplyr::left_join(DiaEven, by = ("TripCode")) %>%
    dplyr::left_join(DinoEven, by = ("TripCode")) %>%
    dplyr::left_join(CTD, by = ("TripCode")) %>%
    dplyr::left_join(MLD, by = ("TripCode")) %>%
    dplyr::left_join(Nuts, by = ("TripCode")) %>%
    dplyr::left_join(Pigments, by = ("TripCode"))


  # %>%
  #  dplyr::left_join(GHRSST %>% dplyr::select(-c("Longitude", "Latitude", "Date")), by = ("TripCode")) %>%
  #  dplyr::left_join(MODIS %>% dplyr::select(-c("Longitude", "Latitude", "Date")), by = ("TripCode")) %>%
  #  dplyr::left_join(Alt %>% dplyr::select(c("TripCode", "GSLA", "GSL", "UCUR", "VCUR")), by = ("TripCode"))

  # fwrite(Indices, file = paste0(outD,.Platform$file.sep,"NRS_Indices.csv"), row.names = FALSE)

  # test table
  # n should be 1, replicates or duplicate samples will have values > 1
  # test <- Indices %>% dplyr::group_by(TripCode) %>% dplyr::summarise(n = n())

  return(Indices)
}
