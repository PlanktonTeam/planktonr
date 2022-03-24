#' Import CPR Data
#'
#' Load CPR Data
#'
#' @param file Filename to retrieve from AODN.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_CPRData("cpr_zooplankton_abundance_copepods_data")
#' @importFrom rlang .data
pr_get_CPRData <- function(file){
  dat <- readr::read_csv(stringr::str_replace(pr_get_site(), "LAYER_NAME", file), na = "", show_col_types = FALSE, comment = "#") %>%
    pr_rename()
}


#' Get CPR trips
#'
#' @return A dataframe with CPR Trips
#' @export
#'
#' @examples
#' df <- pr_get_CPRTrips()
#' @importFrom rlang .data
pr_get_CPRTrips <- function(){
  CPRTrips <- readr::read_csv(paste0(pr_get_site2(), "CPR_Trip.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get CPR samples
#'
#' @param Type A character string on which to filter data (P = Phytoplankton, Z = Zooplankton, B = Biomass)
#'
#' @return A dataframe with CPR Samples
#' @export
#'
#' @examples
#' df <- pr_get_CPRSamps("Z")
#' @importFrom rlang .data
pr_get_CPRSamps <- function(Type = "P"){

  CPRSamps <- readr::read_csv(paste0(pr_get_site2(), "CPR_Samp.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::filter(stringr::str_detect(.data$SampleType, paste(Type, collapse = "|"))) %>%
    dplyr::mutate(Year = lubridate::year(.data$SampleDateUTC),
           Month = lubridate::month(.data$SampleDateUTC),
           Day = lubridate::day(.data$SampleDateUTC),
           Time_24hr = stringr::str_sub(.data$SampleDateUTC, -8, -1)) %>%
    pr_add_bioregions() %>%
    dplyr::select(c(.data$TripCode, .data$Sample, .data$Latitude:.data$SampleDateLocal, .data$Year:.data$BioRegion, .data$PCI, .data$Biomass_mgm3, .data$SampleType))

  if("B" %in% Type){ # Return Biomass if its requested. Otherwise not.
    CPRSamps <- CPRSamps %>%
      dplyr::select(-c(.data$PCI, .data$SampleType))
  } else{
    CPRSamps <- CPRSamps %>%
      dplyr::select(-c(.data$PCI, .data$SampleType, .data$Biomass_mgm3))
  }
}

#' Get CPR Phytoplankton Abundance or Count data
#'
#' @param var A string of either "Abundance" or "Count"
#' @return A dataframe with CPR Phytoplankton Abundance or Count data
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoData()
#' @importFrom rlang .data
pr_get_CPRPhytoData <- function(var = "Abundance"){
  cprPdat <- readr::read_csv(paste0(pr_get_site2(), "CPR_Phyto_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()

  if(stringr::str_detect(var, "Abundance")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$FovCount, .data$SampVol_m3, .data$BioVolume_um3m3))
  } else if(stringr::str_detect(var, "Count")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$BioVolume_um3m3, .data$PhytoAbund_m3))
  } else if(stringr::str_detect(var, "Biovolume")){
    cprPdat <- cprPdat %>%
      dplyr::select(-c(.data$FovCount, .data$SampVol_m3, .data$PhytoAbund_m3))
  } else if(stringr::str_detect(var, "All")){
    cprPdat <- cprPdat # Do nothing. Return all variables
  }

}


#' Get Phyto Change Log
#'
#' @return A dataframe with the CPR Phytoplankton Changelog
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoChangeLog()
#' @importFrom rlang .data
pr_get_CPRPhytoChangeLog <- function(){
  cprPcl <- readr::read_csv(paste0(pr_get_site2(), "CPR_Phyto_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

################################################################################################################################################
#' Get CPR Phyto raw product - Abundance
#'
#' @return A dataframe with CPR Raw Phytoplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRaw()
#' @importFrom rlang .data
pr_get_CPRPhytoRaw <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_abundance_raw_data")

}

#' CPR Phyto HTG product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Higher Trophic Groups
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoHTG()
#' @importFrom rlang .data
pr_get_CPRPhytoHTG <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_abundance_htg_data")
}

#' Get CPR Phyto genus product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoGenus()
#' @importFrom rlang .data
pr_get_CPRPhytoGenus <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_abundance_genus_data")
}

#' Get CPR Phyto species product - Abundance
#'
#' @return A dataframe with CPR Phytoplankton Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoSpecies()
#' @importFrom rlang .data
pr_get_CPRPhytoSpecies <-  function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_abundance_species_data")
}

###############################################################################################################################################
#' Get CPR Phyto raw product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoRawBV()
#' @importFrom rlang .data
pr_get_CPRPhytoRawBV <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_biovolume_raw_data")
}

#' Get CPR Phyto HTG product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoHTGBV()
#' @importFrom rlang .data
pr_get_CPRPhytoHTGBV <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_biovolume_htg_data")
}

#' Get CPR Phyto genus product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoGenusBV()
#' @importFrom rlang .data
pr_get_CPRPhytoGenusBV <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_biovolume_genus_data")
}

#' Get CPR Phyto species product - Biovolume
#'
#' @return A dataframe with CPR Phytoplankton BioVolume - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRPhytoSpeciesBV()
#' @importFrom rlang .data
pr_get_CPRPhytoSpeciesBV <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_phytoplankton_biovolume_species_data")
}

#### CPR Zooplankton #### ################################################################################################################################

#' Get CPR Zooplankton abundance data
#'
#' @param var A string of either "Abundance" or "Count"
#' @return A dataframe with CPR Zooplankton Abundance data
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooData()
#' @importFrom rlang .data
pr_get_CPRZooData <- function(var = "Abundance"){

  cprZdat <- readr::read_csv(paste0(pr_get_site2(), "CPR_Zoop_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()

  if(stringr::str_detect(var, "Abundance")){
    cprZdat <- cprZdat %>%
      dplyr::select(-c(.data$TaxonCount, .data$SampVol_m3))
  } else if(stringr::str_detect(var, "Count")){
    cprZdat <- cprZdat %>%
      dplyr::select(-.data$ZoopAbund_m3)
  }
}

#' Get CPR zooplankton Change Log
#'
#' @return A dataframe with the CPR Zooplankton Changelog
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooChangeLog()
#' @importFrom rlang .data
pr_get_CPRZooChangeLog <- function(){
  cprZcl <- readr::read_csv(paste0(pr_get_site2(), "CPR_Zoop_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get CPR Zoop raw product - Abundance
#'
#' @return A dataframe with Raw CPR Zooplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooRaw()
#' @importFrom rlang .data
pr_get_CPRZooRaw <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_zooplankton_abundance_raw_data")
}

#' Get CPR Zoop HTG product - Abundance
#'
#' @return A dataframe with CPR Zooplankton Abundance - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooHTG()
#' @importFrom rlang .data
pr_get_CPRZooHTG <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_zooplankton_abundance_htg_data")
}

#' CPR Zoop genus product - Abundance
#'
#' @return A dataframe with CPR Zooplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooGenus()
#' @importFrom rlang .data
pr_get_CPRZooGenus <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_zooplankton_abundance_genus_data")
}

#' CPR Zoop copepod product - Abundance
#'
#' @return A dataframe with CPR Copepod Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooCopepod()
#' @importFrom rlang .data
pr_get_CPRZooCopepod <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_zooplankton_abundance_copepods_data")
}

#' Get CPR Zoop Non-Copepod Abundance Data
#'
#' @return A dataframe with CPR Zooplankton (non-copepod) Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_CPRZooNonCopepod()
#' @importFrom rlang .data
pr_get_CPRZooNonCopepod <- function(){
  dat <- planktonr::pr_get_CPRData("cpr_zooplankton_abundance_non_copepods_data")
}
