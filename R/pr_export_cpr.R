#' Create all processed data products for CPR
#'
#' @param outD The directory where the output will be saved.
#'
#' @export
#'
#' @examples
#' pr_export_cpr(".")
#'@importFrom magrittr "%>%"
#'
pr_export_cpr <- function(outD){

  #### CPR Phytoplankton #######################################################################################################################################################
  #### CPR PHYTO RAW ####

  cprRawP <- get_CPRPhytoRaw()
  data.table::fwrite(cprRawP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_RawMat.csv"), row.names = FALSE)
  rm(cprRawP)

  #### CPR PHYTO ABUND HTG ####

  cprHTGP <- get_CPRPhytoHTG()
  data.table::fwrite(cprHTGP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_HTGMat.csv"), row.names = FALSE)
  rm(cprHTGP)

  #### CPR PHYTO ABUND GENUS ####

  cprGenP <- get_CPRPhytoGenus()
  data.table::fwrite(cprGenP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_GenusMat.csv"), row.names = FALSE)
  rm(cprGenP)

  #### CPR PHYTO ABUND SPECIES ####

  cprSpecP <- get_CPRPhytoSpecies()
  data.table::fwrite(cprSpecP, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_SpeciesMat.csv"), row.names = FALSE)
  rm(cprSpecP)

  ###################################################################
  #### CPR PHYTO RAW ####

  cprRawPB <- get_CPRPhytoRawBV()
  data.table::fwrite(cprRawPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolRawMat.csv"), row.names = FALSE)
  rm(cprRawPB)

  #### CPR PHYTO BIOV HTG ####

  cprHTGB <- get_CPRHTGBV()
  data.table::fwrite(cprHTGB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolHTGMat.csv"), row.names = FALSE)
  rm(cprHTGB)

  #### CPR PHYTO BIOV GENUS ####

  cprGenPB <- get_CPRPhytoGenusBV()
  data.table::fwrite(cprGenPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolGenusMat.csv"), row.names = FALSE)
  rm(cprGenPB)

  #### CPR PHYTO BIOV SPECIES ####

  cprSpecPB <- get_CPRPhytoSpeciesBV()
  data.table::fwrite(cprSpecPB, file = paste0(outD,.Platform$file.sep,"CPR_Phyto_BioVolSpeciesMat.csv"), row.names = FALSE)
  rm(cprSpecPB)

  #### CPR Zooplankton #### ################################################################################################################################

  #### CPR ZOOP RAW ####
  cprRawZ <- get_CPRZooRaw()
  data.table::fwrite(cprRawZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_RawMat.csv"), row.names = FALSE)
  rm(cprRawZ)

  ### CPR Zooplankton Sex and stage binned

  cprIdsZ <- get_CPRZooRawSS()
  data.table::fwrite(cprIdsZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_IDsMat.csv"), row.names = FALSE)
  rm(cprIdsZ)

  #### CPR ZOOP HTG ####
  cprHTGZ <- get_CPRZooHTG()
  data.table::fwrite(cprHTGZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_HTGMat.csv"), row.names = FALSE)
  rm(cprHTGZ)

  #### CPR ZOOP GENUS ####
  cprGenZ <- get_CPRZooGenus()
  data.table::fwrite(cprGenZ, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_GenusMat.csv"), row.names = FALSE)
  rm(cprGenZ)

  #### CPR ZOOP COPEPODS ####
  cprCop <- get_CPRZooCopepod()
  data.table::fwrite(cprCop, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_CopesMat.csv"), row.names = FALSE)
  rm(cprCop)

  #### CPR ZOOP NON-COPEPODS ####
  cprnCop <- get_CPRZooNonCopepod()
  data.table::fwrite(cprnCop, file = paste0(outD,.Platform$file.sep,"CPR_Zoop_NoncopesMat.csv"), row.names = FALSE)
  rm(cprnCop)

  #### Create CPR Indices File ####
  cprIndices <- create_indices_cpr()
  data.table::fwrite(cprIndices, file = paste0(outD,.Platform$file.sep,"CPR_Indices.csv"), row.names = FALSE)
  rm(cprIndices)
}
