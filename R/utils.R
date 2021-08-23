## Functions for operating


#' Get location of raw plankton data
#'
#' Internal function to load the location of the raw plankton data files.
#' @return A string with location of raw plankton data
#' @export
#' @examples
#' file_loc <- pr_get_site()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
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
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_ZooInfo <- function(){
  ZInfo <- readr::read_csv(paste0(pr_get_site(), "ZoopInfo.csv"), na = "") %>%
    pr_rename()
}



#' Add StationName to data
#'
#' @param df A dataframe that contains `StationCode` but no `StationName`
#' @return A dataframe with StationName added
#' @export
#'
#' @examples
#' df <- pr_get_NRSStation() %>%
#'     pr_get_StationName()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
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




#' Remove flagged data in df
#'
#' @param df A dataframe containing data with associated flags
#' @return A dataframe with flagged data removed
#' @export
#'
#' @examples
#' df <- data.frame(SST = c(27.4, 28.9, 45), SST_Flag = c(1, 1, 4))
#' df <- pr_apply_flags(df)
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @importFrom rlang .data
pr_apply_flags <- function(df){

  # qc_scheme_short_name,flag_value,flag_meaning,flag_description
  # IMOS IODE,0,No QC performed,The level at which all data enter the working archive. They have not yet been quality controlled
  # IMOS IODE,1,Good data,Top quality data in which no malfunctions have been identified and all real features have been verified during the quality control process
  # IMOS IODE,2,Probably good data,Good data in which some features (probably real) are present but these are unconfirmed. Code 2 data are also data in which minor malfunctions may be present but these errors are small and/or can be successfully corrected without seriously affecting the overall quality of the data.
  # IMOS IODE,3,Bad data that are potentially correctable,Suspect data in which unusual,, and probably erroneous features are observed
  # IMOS IODE,4,Bad data,Obviously erroneous values are observed
  # IMOS IODE,5,Value changed,Altered by a QC Centre,, with original values (before the change) preserved in the history record of the profile. eMII discourage the use of this flag. Where data values must be changed (e.g. smoothing of data sets) we strongly prefer that the original data be retained and an additional variable be added to accommodate the interpolated/corrected data values.
  # IMOS IODE,6,Not used,Flag 6 is reserved for future use
  # IMOS IODE,7,Not used,Flag 7 is reserved for future use
  # IMOS IODE,8,Interpolated value,Indicates that data values are interpolated
  # IMOS IODE,9,Missing value,Indicates that the element is missing

  bad_flags <- c(3, 4, 6, 9)

  var_names <- stringr::str_remove(stringr::str_subset(colnames(df),"_Flag"), "_Flag") # Get all the variables from the df that contain a flag (*_Flag)

  for(v in 1:length(var_names)){ # Loop through and apply the flag to all

    out <- colnames(df)[stringr::str_detect(colnames(df), var_names[v])]
    var_units <- stringr::str_subset(stringr::str_subset(out, "_Flag", negate = TRUE), "_Comments", negate = TRUE) # Find the full variable variable name
    var_flags <- stringr::str_subset(out,"_Flag") # Find the flag

    df <- df %>%
      mutate(!!var_units := ifelse(eval(rlang::sym(var_flags)) %in% bad_flags, NA, eval(rlang::sym(var_units))))
    rm(out, var_units, var_flags)
  }
return(df)
}



#' Remove flagged data in df
#'
#' @param df A dataframe containing time column
#' @return A dataframe with extra date columns
#' @export
#'
#' @examples
#' df <- data.frame(SampleDateLocal = lubridate::now())
#' df <- pr_apply_time(df)
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @importFrom rlang .data
pr_apply_time <- function(df){

  df <- df %>%
    mutate(Year = lubridate::year(.data$SampleDateLocal),
           Month = lubridate::month(.data$SampleDateLocal),
           Day = lubridate::day(.data$SampleDateLocal),
           Time_24hr = stringr::str_sub(.data$SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
           tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE),
           SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(.data$SampleDateLocal, .data$tz, roll = TRUE), "UTC"))

}
