#' Import NRS Data
#'
#' Load NRS station Data
#'
#' @param Type The data of interest: Phytoplankton or Zooplankton
#' @param Variable Variable options are: abundance or biovolume (phytoplankton only)
#' @param Subset Data compilation. Full options are below.
#'
#' @return A dataframe with requested plankton data in long form
#' @export
#' @examples
#' df <- pr_get_NRSData(Type = "phytoplankton", Variable = "abundance", Subset = "raw")
#' @importFrom rlang .data
pr_get_NRSData <- function(Type = "phytoplankton", Variable = "abundance", Subset = "raw"){

  Type = stringr::str_to_lower(Type) # Make sure its lower case
  if (Type == "p"){Type = "phytoplankton"}
  if (Type == "z"){Type = "zooplankton"}

  file = paste("bgc", Type, Variable, Subset, "data", sep = "_")

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
  dat <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
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

  NRSTrip <- readr::read_csv(system.file("extdata", "BGC_Trip.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
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



#' Load zooplankton abundance data
#' @return A dataframe with zooplankton abundance data
#' @export
#'
#' @examples
#' df <- pr_get_NRSZooData()
#' @importFrom rlang .data
pr_get_NRSZooData <- function(){
  dat <- readr::read_csv(system.file("extdata", "BGC_Zoop_Raw.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
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
  dat <- readr::read_csv(system.file("extdata", "BGC_Zoop_ChangeLog.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Get pigments data
#'
#' @return A dataframe with NRS pigment data
#' @export
#'
#' @examples
#' df <- pr_get_NRSPigments()
pr_get_NRSPigments <- function(){
  dat <- readr::read_csv(system.file("extdata", "BGC_Pigments.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
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
  dat <- readr::read_csv(system.file("extdata", "BGC_Picoplankton.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename()
}

