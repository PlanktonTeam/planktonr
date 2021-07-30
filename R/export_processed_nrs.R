## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)
## IMOS plankton data products

## Created: May 2020
## Updated:
## 21 July 2020 (Written to Git)
## 22 September 2020 (Updated data file structure)
## 13th November 2020 (Split CPR and NRS)

#' Create all processed data products
#'
#' @return
#' @export
#'
#' @examples
export_processed_nrs <- function(){

  outD <- "Output"

  #### NRS Phytoplankton ####

  # Bring in all NRS phytoplankton samples, data and changelog

  # NRSPdat <- getNRSPhytoData()
  # NRSPcl <- getNRSPhytoChangeLog()

  #### Raw Phytoplankton ####

  NRSRawP <- getNRSRawPhytoPivot()
  fwrite(NRSRawP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_RawMat.csv"), row.names = FALSE)
  rm(NRSRawP)

  #### Higher Trophic Groups Abund ####

  NRSHTGP <- getNRSPhytoHTG()
  fwrite(NRSHTGP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_HTGMat.csv"), row.names = FALSE)
  rm(NRSHTGP)

  #### Genus Abund ####

  NRSGenP <- getNRSPhytoGenus()
  fwrite(NRSGenP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenP)

  #### Species Abund ####

  NRSSpecP <- getNRSPhytoSpecies()
  fwrite(NRSSpecP, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_SpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecP)

  ###########################################################################################
  #### Raw Phytoplankton Biovolume
  NRSRawPB <- getNRSRawPhytoBV()
  fwrite(NRSRawPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolRawMat.csv"), row.names = FALSE)
  rm(NRSRawPB)

  #### Higher Trophic Groups BioV ####

  NRSHTGPB <- getNRSPhytoHTGBV()
  fwrite(NRSHTGPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolHTGMat.csv"), row.names = FALSE)
  rm(NRSHTGPB)

  #### Genus ####

  NRSGenPB <- getNRSPhytoGenusBV()
  fwrite(NRSGenPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVolGenusMat.csv"), row.names = FALSE)
  rm(NRSGenPB)

  #### Species ####

  NRSSpecPB <- getNRSPhytoSpeciesBV()
  fwrite(NRSSpecPB, file = paste0(outD,.Platform$file.sep,"NRS_Phyto_BioVSpeciesMat.csv"), row.names = FALSE)
  rm(NRSSpecPB)


  #### NRS Zooplankton #### #################################################################################################################################
  # Bring in all NRS zooplankton samples, data and changelog
  NRSZdat <- getNRSZooData()
  NRSZcl <- getNRSZooChangeLog()

  #### Raw Zooplankton ####
  NRSRawZ <- getNRSZooRaw()
  fwrite(NRSRawZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_RawMat.csv"), row.names = FALSE)
  rm(NRSRawZ)

  ### Sex and stage binned
  NRSIdsZ <- getNRSZooRawBin()
  fwrite(NRSIdsZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_IDsMat.csv"), row.names = FALSE)
  rm(NRSIdsZ)

  #### Higher Trophic Groups ####
  nrsHTGZ <- getNRSZooHTG()
  fwrite(nrsHTGZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_HTGMat.csv"), row.names = FALSE)
  rm(nrsHTGZ)

  #### Genus ####

  NRSGenZ <- getNRSZooGenus()
  fwrite(NRSGenZ, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_GenusMat.csv"), row.names = FALSE)
  rm(NRSGenZ)

  #### Copepods ####

  NRSCop <- getNRSZooSpeciesCopepod()
  fwrite(NRSCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_CopesMat.csv"), row.names = FALSE)
  rm(NRSCop)

  #### Non-Copepods ####

  NRSnCop <- getNRSZooSpeciesNonCopepod()
  fwrite(NRSnCop, file = paste0(outD,.Platform$file.sep,"NRS_Zoop_NoncopesMat.csv"), row.names = FALSE)
  rm(NRSnCop)
}
