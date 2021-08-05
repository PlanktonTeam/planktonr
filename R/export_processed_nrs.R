## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)
## IMOS plankton data products

## Created: May 2020
## Updated:
## 21 July 2020 (Written to Git)
## 22 September 2020 (Updated data file structure)
## 13th November 2020 (Split CPR and NRS)

#' Create all processed data products
#'
#' @param outD The directory where the output will be saved.
#'
#' @export
#'
#' @examples
#'
#'@importFrom magrittr "%>%"
export_processed_nrs <- function(outD){

  #### NRS Phytoplankton ####

  # Bring in all NRS phytoplankton samples, data and changelog

  # NRSPdat <- get_NRSPhytoData()
  # NRSPcl <- get_NRSPhytoChangeLog()

  #### Raw Phytoplankton ####

  NRSRawP <- get_NRSRawPhytoPivot()
  data.table::fwrite(NRSRawP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_RawMat.csv"), row.names = FALSE)
  rm(NRSRawP)

  #### Higher Trophic Groups Abund ####

  NRSHTGP <- get_NRSPhytoHTG()
  data.table::fwrite(NRSHTGP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_HTGMat.csv"), row.names = FALSE)
  rm(NRSHTGP)

  #### Genus Abund ####

  NRSGenP <- get_NRSPhytoGenus()
  data.table::fwrite(NRSGenP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenP)

  #### Species Abund ####

  NRSSpecP <- get_NRSPhytoSpecies()
  data.table::fwrite(NRSSpecP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_SpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecP)

  ###########################################################################################
  #### Raw Phytoplankton Biovolume
  NRSRawPB <- get_NRSRawPhytoBV()
  data.table::fwrite(NRSRawPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolRawMat.csv"), row.names = FALSE)
  rm(NRSRawPB)

  #### Higher Trophic Groups BioV ####

  NRSHTGPB <- get_NRSPhytoHTGBV()
  data.table::fwrite(NRSHTGPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolHTGMat.csv"), row.names = FALSE)
  rm(NRSHTGPB)

  #### Genus ####

  NRSGenPB <- get_NRSPhytoGenusBV()
  data.table::fwrite(NRSGenPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolGenusMat.csv"), row.names = FALSE)
  rm(NRSGenPB)

  #### Species ####

  NRSSpecPB <- get_NRSPhytoSpeciesBV()
  data.table::fwrite(NRSSpecPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVSpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecPB)


  #### NRS Zooplankton #### #################################################################################################################################
  # Bring in all NRS zooplankton samples, data and changelog
  NRSZdat <- get_NRSZooData()
  NRSZcl <- get_NRSZooChangeLog()

  #### Raw Zooplankton ####
  NRSRawZ <- get_NRSZooRaw()
  data.table::fwrite(NRSRawZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_RawMat.csv"), row.names = FALSE)
  rm(NRSRawZ)

  ### Sex and stage binned
  NRSIdsZ <- get_NRSZooRawBin()
  data.table::fwrite(NRSIdsZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_IDsMat.csv"), row.names = FALSE)
  rm(NRSIdsZ)

  #### Higher Trophic Groups ####
  nrsHTGZ <- get_NRSZooHTG()
  data.table::fwrite(nrsHTGZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_HTGMat.csv"), row.names = FALSE)
  rm(nrsHTGZ)

  #### Genus ####

  NRSGenZ <- get_NRSZooGenus()
  data.table::fwrite(NRSGenZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenZ)

  #### Copepods ####

  NRSCop <- get_NRSZooSpeciesCopepod()
  data.table::fwrite(NRSCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_CopesMat.csv"), row.names = FALSE)
  rm(NRSCop)

  #### Non-Copepods ####
  NRSnCop <- get_NRSZooSpeciesNonCopepod()
  data.table::fwrite(NRSnCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_NoncopesMat.csv"), row.names = FALSE)
  rm(NRSnCop)

  #### Create NRS Indices File ####
  IndicesNRS <- create_indices_nrs()
  data.table::fwrite(Indices, file = paste0(outD,.Platform$file.sep,"NRS_Indices.csv"), row.names = FALSE)

  #### Create NRS BGC File ####
  IndicesNRS <- create_bgc()
  data.table::fwrite(BGC, file = paste0(outD,.Platform$file.sep,"NRS_CombinedWaterQuality.csv"), row.names = FALSE)

}
