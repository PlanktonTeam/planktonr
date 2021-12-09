#' Import NRS Phytoplankton Data
#'
#' Load NRS station Phytoplankton Data
#'
#' @param file Filename to retrieve from AODN.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_NRSData("bgc_phytoplankton_abundance_htg_data")
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSData <- function(file){
  dat <- readr::read_csv(stringr::str_replace(pr_get_site(), "LAYER_NAME", file), na = "", show_col_types = FALSE, comment = "#") %>%
    pr_rename()
}


#' Import NRS Station information
#'
#' Load NRS station information including codes, location, and sampling interval.
#'
#' @return A dataframe with NRS Station information
#' @export
#' @examples
#' df <- pr_get_NRSStation()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSStation <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_StationInfo.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    filter(.data$ProjectName == "NRS")
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @param Type A character string on which to filter data (P = Phytoplankton, Z = Zooplankton, F = Fish)
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSTrips <- function(Type = c("P","Z","F")){

  NRSTrip <- readr::read_csv(paste0(pr_get_site2(), "BGC_Trip.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    rename(ZSampleDepth_m = .data$ZOOPSAMPLEDEPTH_M,
           PSampleDepth_m = .data$PHYTOSAMPLEDEPTH_M) %>%
    filter(.data$ProjectName == "NRS" &
             (stringr::str_detect(.data$SampleType, paste(Type, collapse = "|")) |
                is.na(.data$SampleType))) %>%
    pr_apply_time() %>%
    select(.data$TripCode:.data$SampleDateLocal, .data$Year:.data$SampleDateUTC, .data$Biomass_mgm3, .data$Secchi_m, .data$SampleType) %>%
    select(-.data$tz)


  if("P" %in% Type & !"Z" %in% Type){ # Only Phytoplankton
    NRSTrip <- NRSTrip %>%
      rename(SampleDepth_m = .data$PSampleDepth_m) %>%
      select(-.data$ZSampleDepth_m)
  }

  if("Z" %in% Type & !"P" %in% Type){ # Only Zooplankton
    NRSTrip <- NRSTrip %>%
      rename(SampleDepth_m = .data$ZSampleDepth_m) %>%
      select(-.data$PSampleDepth_m)
  }

  return(NRSTrip)

}

#
#' Get NRS Phytoplankton raw data - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRaw()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoRaw <- function(){
  dat <- pr_get_NRSData("bgc_phytoplankton_abundance_raw_data")
}


#### Higher Trophic Groups Abund ####
#' Get Abundance of Phyto Higher Trophic Groups
#'
#' @return A dataframe with Phytoplankton Abundance data - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTG()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoHTG <- function(){

  dat <- pr_get_NRSData("bgc_phytoplankton_abundance_htg_data")

}



# Import NRS Phytoplankton Data
#
# Load NRS station Phytoplankton Data
# @return A dataframe with NRS Phytoplankton Data in long form
# @export
# @examples
# df <- pr_get_NRSPhytoData()
# @import dplyr
# @importFrom magrittr "%>%"
# @importFrom rlang .data
# pr_get_NRSPhytoData <- function(){
#   dat <- pr_get_NRSData("anmn_nrs_bgc_plankton_phytoplankton_data")
# }



#' Import NRS Phytoplankton Changelog
#'
#' Load NRS Phytoplankton Changelog
#' @return A dataframe with NRS Phytoplankton Changelog
#' @export
#' @examples
#' df <- pr_get_NRSPhytoChangeLog()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoChangeLog <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Phyto_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



#
#' Get NRS Phytoplankton genus product - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenus()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoGenus <- function() {
  dat <- pr_get_NRSData("bgc_phytoplankton_abundance_genus_data")
}

#' Get NRS Phytoplankton species product - Abundance
#'
#' @return A dataframe with NRS Phytoplankton Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpecies()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoSpecies <- function(){
  dat <- pr_get_NRSData("bgc_phytoplankton_abundance_species_data")
}


#' Get NRS Phytoplankton raw product - Biovolume
#'
#' @return A dataframe with raw NRS Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoRawBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoRawBV <- function(){
  dat <- pr_get_NRSData("bgc_phytoplankton_biovolume_raw_data")
}

#' Get NRS Phytoplankton higher taxon group product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoHTGBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoHTGBV <- function() {
  dat <- pr_get_NRSData("bgc_phytoplankton_biovolume_htg_data")
}

#' Get NRS Phytoplankton genus product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoGenusBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoGenusBV <- function(){
  dat <- pr_get_NRSData("bgc_phytoplankton_biovolume_genus_data")
}

#' Get NRS Phytoplankton species product - Biovolume
#'
#' @return A dataframe with NRS Phytoplankton BioVolume - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSPhytoSpeciesBV()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPhytoSpeciesBV <- function(){
  dat <- pr_get_NRSData("bgc_phytoplankton_biovolume_species_data")
}


#' Load zooplankton abundance data
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooData()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooData <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Zoop_Raw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



#' Load zooplankton NRS Zooplankton changelog
#'
#' @return A dataframe with NRS zooplankton changelog
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooChangeLog()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooChangeLog <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Zoop_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get NRS Zoop raw product - Abundance
#'
#' @return A dataframe with Raw NRS Zooplankton Abundance
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooRaw()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooRaw <- function(){
  dat <- pr_get_NRSData("bgc_zooplankton_abundance_raw_data")
}

# NRS Zoop raw product binned by sex and stage raw product
#
# @return A dataframe with NRS Zooplankton Abundance - Binned by sex and stage
# @export
#
# @examples
# df <- pr_get_NRSZooRawBin()
# @import dplyr
# @importFrom magrittr "%>%"
# @importFrom rlang .data
# pr_get_NRSZooRawBin <- function(){
#   dat <- left_join(pr_get_NRSTrips("Z") %>%
#                      select(-c(.data$Biomass_mgm3, .data$Secchi_m)),
#                    pr_get_NRSZooData(), by = "TripCode") %>%
#     mutate(TaxonName = if_else(is.na(.data$Genus), .data$TaxonName, paste0(.data$Genus, ' ', .data$Species))) %>%
#     group_by(.data$TripCode, .data$StationName, .data$Latitude, .data$Longitude,
#              .data$SampleDateLocal, .data$Year, .data$Month, .data$Day, .data$Time_24hr, .data$SampleDateUTC, .data$TaxonName) %>%
#     summarise(ZoopAbund_m3 = sum(.data$ZoopAbund_m3, na.rm = TRUE)) %>%
#     arrange(-desc(.data$TaxonName))  %>%
#     tidyr::pivot_wider(names_from = .data$TaxonName, values_from = .data$ZoopAbund_m3, values_fill = list(ZoopAbund_m3 = 0)) %>%
#     arrange(desc(.data$SampleDateLocal))
# }


#' Get NRS Zoop HTG product - Abundance
#'
#' @return A dataframe with NRS Zooplankton Abundance - Summed by Higher Trophic Levels
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooHTG()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooHTG <-  function(){
  dat <- pr_get_NRSData("bgc_zooplankton_abundance_htg_data")
}

#' NRS Zoop genus product - Abundance
#'
#' @return A dataframe with NRS Zooplankton Abundance - Summed by Genus
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooGenus()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooGenus <- function(){
  dat <- pr_get_NRSData("bgc_zooplankton_abundance_genus_data")
}

#' NRS Zoop copepod product - Abundance
#'
#' @return A dataframe with NRS Copepod Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooSpeciesCopepod()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooSpeciesCopepod <- function(){
  dat <- pr_get_NRSData("bgc_zooplankton_abundance_copepods_data")
}


#' Get NRS Zoop Non-Copepod Abundance Data
#'
#' @return A dataframe with NRS Zooplankton (non-copepod) Abundance - Summed by Species
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooSpeciesNonCopepod()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSZooSpeciesNonCopepod <- function(){
  dat <- pr_get_NRSData("bgc_zooplankton_abundance_non_copepods_data")
}



#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPigments()
pr_get_NRSPigments <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(),"BGC_Pigments.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Load picophytoplankton data
#'
#' @return A dataframe with NRS picophytoplankton data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_NRSPico <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Picoplankton.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

