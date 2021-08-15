## Functions for operating


#' Get location of raw plankton data
#'
#' Internal function to load the location of the raw plankton data files.
#' @return A string with location of raw plankton data
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' file_loc <- pr_get_site()
pr_get_site <- function(){
  raw <- "https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/"
}


#' Load copepod information table with sizes etc.
#'
#' @return A dataframe with NRS zooplankton information
#' @export
#'
#' @examples
#' df <- pr_get_ZooInfo()

pr_get_ZooInfo <- function(){
  ZInfo <- readr::read_csv(paste0(pr_get_site(), "ZoopInfo.csv"), na = "") %>%
    pr_rename()
}



#' Add StationName to data
#'
#' @return A dataframe with StationName added
#' @export
#'
#' @examples

pr_get_StationName <- function(df){
  df <- df %>%
    mutate(StationName = case_when(
      StationCode == "NRSDAR" ~ "Darwin",
      StationCode == "NRSYON" ~ "Yongala",
      StationCode == "NRSNSI" ~ "North Stradbroke Island",
      StationCode == "NRSPHB" ~ "Port Hacking",
      StationCode == "NRSMAI" ~ "Maria Island",
      StationCode == "NRSKAI" ~ "Kangaroo Island",
      StationCode == "NRSESP" ~ "Esperance",
      StationCode == "NRSROT" ~ "Rottnest Island",
      StationCode == "NRSNIN" ~ "Ningaloo"))
}

