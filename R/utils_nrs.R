#' Import NRS Data
#'
#' Load NRS station Data
#'
#' @param file Filename to retrieve from AODN.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_NRSData(type = "phytoplankton", variable = "abundance", subset = "raw")
#' @importFrom rlang .data
pr_get_NRSData <- function(type = "phytoplankton", variable = "abundance", subset = "raw"){

  file = paste("bgc", type, variable, subset, "data", sep = "_")

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
#' @importFrom rlang .data
pr_get_NRSStation <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_StationInfo.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::filter(.data$ProjectName == "NRS")
}


#' Import NRS BGC information
#'
#' Load NRS station BGC information
#' @param Type A character string on which to filter data (P = Phytoplankton, Z = Zooplankton, F = Fish)
#' @return A dataframe with NRS BGC information
#' @export
#' @examples
#' df <- pr_get_NRSTrips()
#' @importFrom rlang .data
pr_get_NRSTrips <- function(Type = c("P","Z","F")){

  NRSTrip <- readr::read_csv(paste0(pr_get_site2(), "BGC_Trip.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::rename(ZSampleDepth_m = .data$ZOOPSAMPLEDEPTH_M,
                  PSampleDepth_m = .data$PHYTOSAMPLEDEPTH_M) %>%
    dplyr::filter(.data$ProjectName == "NRS" &
                    (stringr::str_detect(.data$SampleType, paste("P", collapse = "|")) |
                       is.na(.data$SampleType))) %>%
    pr_apply_time() %>%
    dplyr::select(-c(.data$tz, .data$ProjectName))


  if("P" %in% Type & !"Z" %in% Type){ # Only Phytoplankton
    NRSTrip <- NRSTrip %>%
      dplyr::rename(SampleDepth_m = .data$PSampleDepth_m) %>%
      dplyr::select(-.data$ZSampleDepth_m)
  }

  if("Z" %in% Type & !"P" %in% Type){ # Only Zooplankton
    NRSTrip <- NRSTrip %>%
      dplyr::rename(SampleDepth_m = .data$ZSampleDepth_m) %>%
      dplyr::select(-.data$PSampleDepth_m)
  }

  return(NRSTrip)

}




# Import NRS Phytoplankton Data
#
# Load NRS station Phytoplankton Data
# @return A dataframe with NRS Phytoplankton Data in long form
# @export
# @examples
# df <- pr_get_NRSPhytoData()
# @importFrom rlang .data
# pr_get_NRSPhytoData <- function(){
#    dat <- pr_get_NRSData("anmn_nrs_bgc_plankton_phytoplankton_data")
# }



# Import NRS Phytoplankton Changelog
#
# Load NRS Phytoplankton Changelog
# @return A dataframe with NRS Phytoplankton Changelog
# @export
# @examples
# df <- pr_get_NRSPhytoChangeLog()
# @importFrom rlang .data
# pr_get_NRSPhytoChangeLog <- function(){
#  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Phyto_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
#    pr_rename()
#}



#' Load zooplankton abundance data
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooData()
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
#' @importFrom rlang .data
pr_get_NRSZooChangeLog <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Zoop_ChangeLog.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}



# Get NRS Zoop Species Abundance Data
#
# @return A dataframe with NRS Zooplankton Abundance - Summed by Species
# @export
#
# @examples
# df <- pr_get_NRSZooSpecies()
# pr_get_NRSZooSpecies <- function(){
#   dat <- pr_get_NRSData(type = "zooplankton", variable = "abundance", subset = "copepods") %>%
#     dplyr::select(-c("Project", "StationName", "StationCode", "Latitude",
#                      "Longitude", "SampleTime_local", "Year", "Month",
#                      "Day", "Time", "SampleDepth_m", "Biomass_mgm3")) %>%
#     dplyr::left_join(pr_get_NRSData("bgc_zooplankton_abundance_non_copepods_data"), ., by = "TripCode")
# }


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
#' @importFrom rlang .data
pr_get_NRSPico <- function(){
  dat <- readr::read_csv(paste0(pr_get_site2(), "BGC_Picoplankton.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

