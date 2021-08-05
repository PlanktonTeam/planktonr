## IMOS plankton data products
## Claire Davies (CSIRO) and Jason D Everett (UQ/CSIRO)

## Created: May 2020
## Updated:
## 21 July 2020 (Written to Git)
## 22 September 2020 (Updated data file structure)
## 13th November 2020 (Split CPR and NRS)

#' Create all processed data products for CPR
#'
#' @param outD
#'
#' @return
#' @export
#'
#' @examples
#'
#'@importFrom magrittr "%>%"
#'
export_processed_cpr <- function(outD){

  #### CPR Phytoplankton #######################################################################################################################################################
  #### CPR PHYTO RAW ####

  cprRawP <- get_CPRPhytoRaw()
  fwrite(cprRawP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_RawMat.csv", row.names = FALSE))
  rm(cprRawP)

  #### CPR PHYTO ABUND HTG ####

  cprHTGP <- get_CPRPhytoHTG()
  fwrite(cprHTGP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_HTGMat.csv", row.names = FALSE))
  rm(cprHTGP)

  #### CPR PHYTO ABUND GENUS ####

  cprGenP <- get_CPRPhytoGenus()
  fwrite(cprGenP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_GenusMat.csv", row.names = FALSE))
  rm(cprGenP)

  #### CPR PHYTO ABUND SPECIES ####

  cprSpecP <- get_CPRPhytoSpecies()
  fwrite(cprSpecP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_SpeciesMat.csv", row.names = FALSE))
  rm(cprSpecP)

  ###################################################################
  #### CPR PHYTO RAW ####

  cprRawPB <- get_CPRPhytoRawBV()
  fwrite(cprRawPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolRawMat.csv", row.names = FALSE))
  rm(cprRawPB)

  #### CPR PHYTO BIOV HTG ####

  cprHTGB <- get_CPRHTGBV()
  fwrite(cprHTGB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolHTGMat.csv", row.names = FALSE))
  rm(cprHTGPB1)

  #### CPR PHYTO BIOV GENUS ####

  cprGenPB <- get_CPRPhytoGenusBV()
  fwrite(cprGenPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolGenusMat.csv", row.names = FALSE))
  rm(cprGenPB)

  #### CPR PHYTO BIOV SPECIES ####

  cprSpecPB <- get_CPRPhytoSpeciesBV()
  fwrite(cprSpecPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolSpeciesMat.csv", row.names = FALSE))
  rm(cprSpecPB)

  #### CPR Zooplankton #### ################################################################################################################################

  #### CPR ZOOP RAW ####
  cprRawZ <- get_CPRZooRaw()
  fwrite(cprRawZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_RawMat.csv", row.names = FALSE))
  rm(cprRawZ)

  ### CPR Zooplankton Sex and stage binned

  cprIdsZ <- get_CPRZooRawSS()
  fwrite(cprIdsZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_IDsMat.csv", row.names = FALSE))
  rm(cprIdsZ)

  #### CPR ZOOP HTG ####
  cprHTGZ <- get_CPRZooHTG()
  fwrite(cprHTGZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_HTGMat.csv", row.names = FALSE))
  rm(cprHTGZ)

  #### CPR ZOOP GENUS ####
  cprGenZ <- get_CPRZooGenus()
  fwrite(cprGenZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_GenusMat.csv", row.names = FALSE))
  rm(cprGenZ)

  #### CPR ZOOP COPEPODS ####
  cprCop <- get_CPRZooCopepod()
  fwrite(cprCop, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_CopesMat.csv", row.names = FALSE))
  rm(cprCop)

  #### CPR ZOOP NON-COPEPODS ####
  cprnCop <- get_CPRZooNonCopepod()
  fwrite(cprnCop, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_NoncopesMat.csv", row.names = FALSE))
  rm(cprnCop)

  #### Create CPR Indices File ####
  IndicesCPR <- create_indices_cpr()
  fwrite(IndicesCPR, file = paste0("Output/",.Platform$file.sep, "CPR_Indices.csv"), row.names = FALSE)
}
