#' Import CPR Data
#'
#' Load CPR Data
#'
#' cpr_phytoplankton_abundance_raw_data
#' cpr_phytoplankton_abundance_htg_data
#' cpr_phytoplankton_abundance_genus_data
#' cpr_phytoplankton_abundance_species_data
#'
#' cpr_phytoplankton_biovolume_raw_data
#' cpr_phytoplankton_biovolume_htg_data
#' cpr_phytoplankton_biovolume_genus_data
#' cpr_phytoplankton_biovolume_species_data
#'
#' cpr_zooplankton_abundance_copepods_data
#' cpr_zooplankton_abundance_non_copepods_data
#' cpr_zooplankton_abundance_genus_data
#' cpr_zooplankton_abundance_htg_data
#' cpr_zooplankton_abundance_raw_data
#'
#' @param type The data of interest: Phytoplankton or Zooplankton
#' @param variable Variable options are: abundance or biovolume (phytoplankton only)
#' @param subset Data compilation. Full options are below.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_CPRData(type = "phytoplankton", variable = "abundance", subset = "raw")
#' @importFrom rlang .data
pr_get_CPRData <- function(type = "phytoplankton", variable = "abundance", subset = "raw"){

file = paste("cpr", type, variable, subset, "data", sep = "_")

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
