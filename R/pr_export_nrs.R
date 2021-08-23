#' Create all processed data products
#'
#' @param outD The directory where the output will be saved.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pr_export_nrs("Output")
#' }
#'
#'@importFrom magrittr "%>%"
pr_export_nrs <- function(outD){

  #### NRS Phytoplankton ####  #################################################################################

  #### Raw Phytoplankton ####
  NRSRawP <- pr_get_NRSRawPhytoPivot()
  data.table::fwrite(NRSRawP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_RawMat.csv"), row.names = FALSE)
  rm(NRSRawP)

  #### Higher Trophic Groups Abund ####
  NRSHTGP <- pr_get_NRSPhytoHTG()
  data.table::fwrite(NRSHTGP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_HTGMat.csv"), row.names = FALSE)
  rm(NRSHTGP)

  #### Genus Abund ####
  NRSGenP <- pr_get_NRSPhytoGenus()
  data.table::fwrite(NRSGenP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenP)

  #### Species Abund ####
  NRSSpecP <- pr_get_NRSPhytoSpecies()
  data.table::fwrite(NRSSpecP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_SpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecP)

  #### Raw Phytoplankton Biovolume
  NRSRawPB <- pr_get_NRSPhytoRawBV()
  data.table::fwrite(NRSRawPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolRawMat.csv"), row.names = FALSE)
  rm(NRSRawPB)

  #### Higher Trophic Groups BioV ####
  NRSHTGPB <- pr_get_NRSPhytoHTGBV()
  data.table::fwrite(NRSHTGPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolHTGMat.csv"), row.names = FALSE)
  rm(NRSHTGPB)

  #### Genus ####
  NRSGenPB <- pr_get_NRSPhytoGenusBV()
  data.table::fwrite(NRSGenPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolGenusMat.csv"), row.names = FALSE)
  rm(NRSGenPB)

  #### Species ####
  NRSSpecPB <- pr_get_NRSPhytoSpeciesBV()
  data.table::fwrite(NRSSpecPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVSpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecPB)


  #### NRS Zooplankton #### #################################################################################

  #### Raw Zooplankton ####
  NRSRawZ <- pr_get_NRSZooData()
  data.table::fwrite(NRSRawZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_RawMat.csv"), row.names = FALSE)
  rm(NRSRawZ)

  ### Sex and stage binned
  NRSIdsZ <- pr_get_NRSZooRawBin()
  data.table::fwrite(NRSIdsZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_IDsMat.csv"), row.names = FALSE)
  rm(NRSIdsZ)

  #### Higher Trophic Groups ####
  nrsHTGZ <- pr_get_NRSZooHTG()
  data.table::fwrite(nrsHTGZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_HTGMat.csv"), row.names = FALSE)
  rm(nrsHTGZ)

  #### Genus ####
  NRSGenZ <- pr_get_NRSZooGenus()
  data.table::fwrite(NRSGenZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenZ)

  #### Copepods ####
  NRSCop <- pr_get_NRSZooSpeciesCopepod()
  data.table::fwrite(NRSCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_CopesMat.csv"), row.names = FALSE)
  rm(NRSCop)

  #### Non-Copepods ####
  NRSnCop <- pr_get_NRSZooSpeciesNonCopepod()
  data.table::fwrite(NRSnCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_NoncopesMat.csv"), row.names = FALSE)
  rm(NRSnCop)

  #### Create NRS Indices File ####
  NRSIndices <- pr_get_indices_nrs()
  data.table::fwrite(NRSIndices, file = paste0(outD,.Platform$file.sep,"NRS_Indices.csv"), row.names = FALSE)
  rm(NRSIndices)

  #### Create NRS BGC File ####
  NRSBGC <- pr_get_bgc()
  data.table::fwrite(NRSBGC, file = paste0(outD,.Platform$file.sep,"NRS_CombinedWaterQuality.csv"), row.names = FALSE)
  rm(NRSBGC)

  ## Still to Add in CTD. Unsure if this should be with/without the drop_na, missingCode, NRSaddCTD etc
  #
  # CTD <- getCTD() %>% drop_na(TripCode)
  # write_csv(CTD, "RawData/NRS_CTD.csv")
  #
  # missingCode <- rawCTD %>% filter(is.na(TripCode)) %>% select(StationName, CastTime_UTC, file_id) %>% unique()
  #
  # NRSaddCTD <- NRSSamp %>% left_join(df, by = 'TripCode')
  # NRSmissingCTD <- NRSaddCTD %>% filter(is.na(file_id))

}
