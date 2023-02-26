# Get location of raw plankton data
#
# Internal function to load the location of the raw plankton data files.
# @return A string with location of raw plankton data
# @export
# @examples
# file_loc <- pr_get_Site()
# @importFrom rlang .data
# pr_get_Site_bgc <- function(){
#   # raw <-"http://geoserver-123.aodn.org.au/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=imos:LAYER_NAME&outputFormat=csv-with-metadata-header"
#   raw = "https://geoserver-portal.aodn.org.au/geoserver/ows?typeName=LAYER_NAME&SERVICE=WFS&outputFormat=csv&REQUEST=GetFeature&VERSION=1.0.0&userId=Guest"
# }

#' Get location of raw plankton data
#'
#' Internal function to load the location of the raw plankton data files.
#' @return A string with location of raw plankton data
#' @export
#' @examples
#' file_loc <- pr_get_Site()
#' @importFrom rlang .data
pr_get_Site <- function(){
  raw <-"http://geoserver-123.aodn.org.au/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=imos:LAYER_NAME&outputFormat=csv-with-metadata-header"
  # raw = "https://geoserver-portal.aodn.org.au/geoserver/ows?typeName=LAYER_NAME&SERVICE=WFS&outputFormat=csv&REQUEST=GetFeature&VERSION=1.0.0&userId=Guest"
}


#' Get location of s3 plankton data
#'
#' Internal function to load the location of the s3 plankton data files. Browse files here:
#' http://imos-data.s3-website-ap-southeast-2.amazonaws.com/?prefix=IMOS/BGC_DB/harvested_from_CSIRO/
#' @return A string with location of s3 plankton data
#' @export
#' @examples
#' file_loc <- pr_get_s3site()
#' @importFrom rlang .data
pr_get_s3site <- function(){
  raw <-"https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/"
}

#' Download Raw Data from IMOS
#'
#' This function is not intended for use by most people. It downloads the raw data file from IMOS with NO QC or data manipulation done. User beware. It can be used to view the raw data that IMOS provides behind the scenes.
#'
#'Options for `file` include: "bgc_chemistry_data", "bgc_pigments_data", "bgc_picoplankton_data", "bgc_tss_data".
#'
#' @param file The filename of the requested data
#'
#' @return A dataframe of raw IMOS data
#' @export
#'
#' @examples
#' dat <- pr_get_Raw("bgc_chemistry_data")
#' dat <- pr_get_Raw("bgc_pigments_data")
#' dat <- pr_get_Raw("bgc_picoplankton_data")
#' dat <- pr_get_Raw("bgc_tss_data")
#'
pr_get_Raw <- function(file){
  # stringr::str_detect(file, "bgc_"
  if (file == "bgc_chemistry_data"){ # additional probs possible in chemistry. No WC.
    col_types = list(Project = readr::col_character(),
                     TripCode = readr::col_character(),
                     SampleTime_Local = readr::col_datetime(),
                     DIC_umolkg = readr::col_double(),
                     Oxygen_umolL = readr::col_double())
  } else if (file == "bgc_tss_data"){ # Add specific filter for bgc files to deal with potential problems from 'WC' depth
    col_types = list(Project = readr::col_character(),
                     TripCode = readr::col_character(),
                     SampleTime_Local = readr::col_datetime(),
                     SampleDepth_m = readr::col_character())
  } else if (file == "bgc_picoplankton_data"){ # Add specific filter for bgc files to deal with potential problems from 'WC' depth
    col_types = list(Project = readr::col_character(),
                     TripCode = readr::col_character(),
                     SampleTime_Local = readr::col_datetime(),
                     SampleDepth_m = readr::col_character(),
                     Virus_cellsmL = readr::col_number(),
                     Virus_flag = readr::col_number())
  } else if (file ==  "bgc_pigments_data"){ # Add specific filter for bgc files to deal with potential problems from 'WC' depth
    col_types = list(Project = readr::col_character(),
                     TripCode = readr::col_character(),
                     SampleTime_Local = readr::col_datetime(),
                     SampleDepth_m = readr::col_character())
  } else if(stringr::str_detect(file, "bgc_zooplankton")){
    col_types = list(Project = readr::col_character(),
                     TripCode = readr::col_character(),
                     SampleTime_Local = readr::col_datetime(),
                     SampleDepth_m = readr::col_character())
  } else {
    col_types = list()
  }

  # if (file ==  "bgc_pigments_data" | file == "bgc_tss_data" |
  #     file == "bgc_picoplankton_data" | file == "bgc_chemistry_data"){
  #   dat <- readr::read_csv(stringr::str_replace(
  #     pr_get_Site_bgc(), "LAYER_NAME", file),
  #     na = c("", NaN, "NaN", NA, "NA"),
  #     show_col_types = FALSE,
  #     comment = "#",
  #     col_types = col_types)
  # } else{
  dat <- readr::read_csv(stringr::str_replace(
    pr_get_Site(), "LAYER_NAME", file),
    na = c("", NaN, "NaN", NA, "NA"),
    show_col_types = FALSE,
    comment = "#",
    col_types = col_types)
  # }

  return(dat)
}

#' Download Raw Data from IMOS
#'
#' This function is not intended for use by most people. It downloads the raw data file from IMOS with NO QC or data manipulation done. User beware. It can be used to view the raw data that IMOS provides behind the scenes.
#'
#' Options for `file` include: "bgc_trip"
#'
#' @param file The filename of the requested data
#'
#' @return A dataframe of raw IMOS data
#' @export
#'
#' @examples
#' dat <- pr_get_s3("bgc_trip")
#' dat <- pr_get_s3("bgc_zoop_raw")
#' dat <- pr_get_s3("bgc_phyto_raw")
#' dat <- pr_get_s3("cpr_phyto_raw")
#' dat <- pr_get_s3("cpr_zoop_raw")
pr_get_s3 <- function(file){

  col_types = list()

  # The file extension is not needed here.
  if(stringr::str_detect(file, ".csv")){
    file = stringr::str_remove(file, ".csv")
  }

  dat <- readr::read_csv(paste0(pr_get_s3site(), file, ".csv"),
                         na = c("", NaN),
                         show_col_types = FALSE,
                         comment = "#",
                         col_types = col_types)

  return(dat)
}


# Add the SampleDate to dataframes that only have SampleTime
#
# @param df Dataframe containing `SampleTime_Local` or `SampleTime_UTC`
#
# @return A dataframe with `SampleDate` included.
# @export
#
# @examples
# df <- # pr_add_SampleDate(data.frame(SampleTime_Local = Sys.time()))
# # pr_add_SampleDate <- function(df){
#
#   if("SampleTime_Local" %in% colnames(df)) {
#     df <- df %>%
#       dplyr::mutate(SampleDate_Local = lubridate::as_date(.data$SampleTime_Local)) %>%
#       dplyr::relocate(.data$SampleDate_Local, .after = .data$SampleTime_Local)
#   }
#
#   if("SampleTime_UTC" %in% colnames(df)) {
#     df <- df %>%
#       dplyr::mutate(SampleDate_UTC = lubridate::as_date(.data$SampleTime_UTC)) %>%
#       dplyr::relocate(.data$SampleDate_UTC, .after = .data$SampleTime_UTC)
#   }
#
#   return(df)
# }





#' Load plankton information table with sizes etc.
#'
#' @param Type Get info for Phytoplankton (`P`) or Zooplankton(`Z`)
#'
#' @return A dataframe with NRS zooplankton information
#' @export
#'
#' @examples
#' df <- pr_get_PlanktonInfo(Type = "P")
#' df <- pr_get_PlanktonInfo(Type = "Z")
#' @importFrom rlang .data
pr_get_PlanktonInfo <- function(Type = "Z"){

  if (Type == "Z"){
    df <- pr_get_s3("zoopinfo") %>%
      pr_rename()
  } else if (Type == "P"){
    df <- pr_get_s3("phytoinfo") %>%
      pr_rename()
  }
  return(df)
}



#' Add NRS StationName to data
#'
#' @param df A dataframe that contains `StationCode` but no `StationName`
#' @return A dataframe with StationName added
#' @export
#'
#' @examples
#' df <- pr_get_NRSStation() %>%
#'     pr_add_StationName()
#' @importFrom rlang .data
pr_add_StationName <- function(df){
  df <- df %>%
    dplyr::mutate(StationName = dplyr::case_when(
      StationCode == "DAR" ~ "Darwin",
      StationCode == "YON" ~ "Yongala",
      StationCode == "NSI" ~ "North Stradbroke Island",
      StationCode == "PHB" ~ "Port Hacking",
      StationCode == "MAI" ~ "Maria Island",
      StationCode == "KAI" ~ "Kangaroo Island",
      StationCode == "ESP" ~ "Esperance",
      StationCode == "ROT" ~ "Rottnest Island",
      StationCode == "NIN" ~ "Ningaloo")) %>%
    dplyr::relocate("StationCode", .after = "StationName")
}



#' Add NRS StationCode to data
#'
#' @param df A dataframe that contains `StationName` but no `StationCode`
#' @return A dataframe with StationCode added
#' @export
#'
#' @examples
#' df <- pr_get_NRSStation() %>%
#'     pr_add_StationCode()
#' @importFrom rlang .data
pr_add_StationCode <- function(df){

  if("StationName" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(StationCode = dplyr::case_when(
        StationName == "Darwin" ~ "DAR",
        StationName == "Yongala" ~ "YON",
        StationName == "North Stradbroke Island" ~ "NSI",
        StationName == "North Stradbroke" ~ "NSI",
        StationName == "Port Hacking" ~ "PHB",
        StationName == "Maria Island" ~ "MAI",
        StationName == "Kangaroo Island" ~ "KAI",
        StationName == "Esperance" ~ "ESP",
        StationName == "Rottnest Island" ~ "ROT",
        StationName == "Ningaloo" ~ "NIN")) %>%
      dplyr::relocate("StationCode", .after = "StationName")
  } else if("TripCode" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(StationCode = stringr::str_sub(.data$TripCode, start = 1, end = 3)) %>%
      dplyr::relocate("StationCode", .after = "TripCode")
  }
  return(df)
}






#' Order the Station or region in the df by latitude for consistent plotting
#'
#' @param df A dataframe that contains StationName, StationCode or BioRegion
#'
#' @return An ordered dataframe
#' @export
#'
#' @examples
#' df <- data.frame(StationName = c('Port Hacking', 'Maria Island',
#' 'North Stradbroke Island','Esperance', 'Ningaloo', "Darwin",
#' 'Rottnest Island',  'Kangaroo Island', "Yongala"))
#' df <- pr_reorder(df)
pr_reorder <- function(df){
  if("StationName" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(StationName = factor(.data$StationName,
                                         levels = c("Darwin", "Yongala", "Ningaloo", "North Stradbroke Island",
                                                    "Rottnest Island", "Esperance", "Port Hacking", "Kangaroo Island",
                                                    "Maria Island")))
  }

  if("StationCode" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(StationCode = factor(.data$StationCode,
                                         levels = c("DAR", "YON", "NIN", "NSI", "ROT",
                                                    "ESP", "PHB", "KAI", "MAI")))
  }
  if("BioRegion" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(BioRegion = factor(.data$BioRegion,
                                       levels = c("North", "North-west", "Coral Sea",
                                                  "Temperate East", "South-east", "South-west",
                                                  "Southern Ocean Region", "None")))
  }
  return(df)
}


#' Remove flagged data in df
#'
#' @param df A dataframe containing data with associated flags
#' @param flag_col A string specifying the column with the flag. Optional. If specified, all rows will be filter by the flag.
#' @return A dataframe with flagged data removed
#' @export
#'
#' @examples
#' df <- data.frame(SST = c(27.4, 28.9, 45), SST_Flag = c(1, 1, 4))
#' df <- pr_apply_Flags(df)
#' @importFrom data.table ":="
#' @importFrom rlang .data
pr_apply_Flags <- function(df, flag_col){

  # qc_scheme_short_name,flag_value,flag_meaning,flag_description
  # IMOS IODE,0,No QC performed,The level at which all data enter the working archive. They have not yet been quality controlled
  # IMOS IODE,1,Good data,Top quality data in which no malfunctions have been identified and all real features have been verified during the quality control process
  # IMOS IODE,2,Probably good data,Good data in which some features (probably real) are present but these are unconfirmed. Code 2 data are also data in which minor malfunctions may be present but these errors are small and/or can be successfully corrected without seriously affecting the overall quality of the data.
  # IMOS IODE,3,Bad data that are potentially correctable,Suspect data in which unusual,, and probably erroneous features are observed
  # IMOS IODE,4,Bad data,Obviously erroneous values are observed
  # IMOS IODE,5,Value changed,Altered by a QC Centre,, with original values (before the change) preserved in the history record of the profile. eMII discourage the use of this flag. Where data values must be changed (e.g. smoothing of data sets) we strongly prefer that the original data be retained and an additional variable be added to accommodate the interpolated/corrected data values.
  # IMOS IODE,6,Under detection limit
  # IMOS IODE,7,Not used,Flag 7 is reserved for future use
  # IMOS IODE,8,Interpolated value,Indicates that data values are interpolated
  # IMOS IODE,9,Missing value,Indicates that the element is missing

  bad_flags <- c(3, 4, 9)

  if (missing(flag_col)){

    # Find and filter flagged data individually
    var_names <- stringr::str_remove(stringr::str_subset(colnames(df),"(?i)_Flag"), "(?i)_Flag") # Get all the variables from the df that contain a flag (*_Flag)

    for(v in 1:length(var_names)){ # Loop through and apply the flag to all

      out <- colnames(df)[stringr::str_detect(colnames(df), var_names[v])]
      var_units <- stringr::str_subset(stringr::str_subset(out, "(?i)_Flag", negate = TRUE), "(?i)_Comments", negate = TRUE) # Find the full variable variable name
      var_flags <- stringr::str_subset(out,"(?i)_Flag") # Find the flag

      df <- df %>%
        dplyr::mutate(!!var_units := dplyr::if_else(eval(rlang::sym(var_flags)) %in% bad_flags, NA_real_, eval(rlang::sym(var_units))))
      rm(out, var_units, var_flags)
    }
  } else {
    # Filter all rows based on 1 flagged column
    df <- df %>%
      dplyr::filter(!(eval(rlang::sym(flag_col)) %in% bad_flags))
  }
  return(df)
}



#' Add Month & year columns to df
#'
#' @param df A dataframe containing time column
#' @return A dataframe with extra date columns
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- pr_get_Indices("NRS", "P") %>%
#'   pr_apply_Time()
pr_apply_Time <- function(df){

  df <- df %>%
    dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local),
                  Month_Local = lubridate::month(.data$SampleTime_Local),
                  # Day_Local = lubridate::day(.data$SampleTime_Local),
                  tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE)) %>%
    dplyr::relocate(tidyselect::all_of(c("Year_Local", "Month_Local", "tz")), .after = "SampleTime_Local")

}

#' Removing outliers from data
#'
#' @param df data frame with Parameters, Values and StationCode or BioRegion
#' @param x no of sd from mean to remove
#'
#' @return df
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "Z") %>% pr_remove_outliers(2)
#' df <- pr_get_Indices("CPR", "Z") %>% pr_remove_outliers(2)
pr_remove_outliers <- function(df, x){

  if("StationCode" %in% colnames(df)){
    location <- rlang::sym("StationCode")
    joiner <- 'StationCode'
  } else {
    location <- rlang::sym("BioRegion")
    joiner <- 'BioRegion'
  }

  df <- df %>% dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values))

  outliers <- df %>%
    dplyr::group_by(.data$Parameters, !!location) %>%
    dplyr::summarise(means = mean(.data$Values, na.rm = TRUE),
                     sd2 = 2*sd(.data$Values, na.rm = TRUE),
                     meanplus = .data$means + .data$sd2,
                     meanminus = .data$means - .data$sd2,
                     .groups = 'drop') %>%
    dplyr::select(-c("means", "sd2"))

  added <- df %>%
    dplyr::left_join(outliers, by = c("Parameters", joiner)) %>%
    dplyr::filter(.data$Values < .data$meanplus & .data$Values > .data$meanminus) %>%
    dplyr::select(-c("meanplus", "meanminus"))
}


#' Remove incorrect species names from dataframe
#'
#' @param df A dataframe with species names
#'
#' @return A dataframe with all correct species names
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- data.frame(Species = c("IncorrectSpecies cf.", "CorrectSpecies1", NA,
#'               "CorrectSpecies2", "Incorrect spp., Incorrect/Species"))
#' df <- pr_filter_Species(df)
pr_filter_Species <- function(df){
  pat <- c("spp.", "cf.", "/", "grp", "complex", "type")
  df <- df %>%
    dplyr::filter(stringr::str_detect(.data$Species, paste(pat, collapse = "|"), negate = TRUE))
}


#' Add Carbon concentration to phytoplankton dataframe
#'
#' This is where you write the description
#' @param df Input dataframe with BioVolume
#' @param meth Method for data collection
#'
#' @return Dataframe with Carbon included
#' @export
#'
#' @examples
#' df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
#'                           Biovolume_um3L = c(100, 150), Cells_L = c(10, 8))
#' df <- pr_add_Carbon(df, "NRS")
#' df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
#'                           BioVolume_um3m3 = c(100, 150), PhytoAbund_m3 = c(10, 8))
#' df <- pr_add_Carbon(df, "CPR")
pr_add_Carbon <- function(df, meth){

  if (meth %in% "CPR"){
    df <- df %>%
      dplyr::mutate(BV_Cell = .data$BioVolume_um3m3 / .data$PhytoAbund_m3, # biovolume of one cell
                    Carbon = dplyr::case_when(.data$TaxonGroup == "Dinoflagellate" ~ 0.76*(.data$BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                              .data$TaxonGroup == 'Ciliate' ~ 0.22*(.data$BV_Cell)^0.939,
                                              .data$TaxonGroup == 'Cyanobacteria'~ 0.2,
                                              TRUE ~ 0.288*(.data$BV_Cell)^0.811),
                    Carbon_m3 = .data$PhytoAbund_m3 * .data$Carbon) # Carbon per m3
    return(df)
  }

  if (meth %in% "NRS"){
    df <- df %>%
      dplyr::mutate(BV_Cell = .data$Biovolume_um3L / .data$Cells_L, # biovolume of one cell
                    Carbon = dplyr::case_when(.data$TaxonGroup == "Dinoflagellate" ~ 0.76*(.data$BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                              .data$TaxonGroup == "Ciliate" ~ 0.22*(.data$BV_Cell)^0.939,
                                              .data$TaxonGroup == "Cyanobacteria" ~ 0.2,
                                              TRUE ~ 0.288*(.data$BV_Cell)^0.811),
                    Carbon_L = .data$Cells_L * .data$Carbon) # Carbon per litre
    return(df)
  }

}



# Add local Time
#
# This function adds the local time based on timezone.
# @param df Dataframe with UTC Time
#
# @return A datafrane with local time added
# @export
#
# @examples
# df <- data.frame(tz = c("Australia/Perth", "Australia/Brisbane"),
#                SampleTime_UTC = c(lubridate::now(), lubridate::now()))
# df <- pr_add_LocalTime(df)
# pr_add_LocalTime <- function(df){
#   df <- purrr::map2(df$SampleTime_UTC, df$tz, function(x,y) lubridate::with_tz(x, tzone = y))
# }

#' For use in models to create a circular predictor
#'
#' This function is for use in models to create a circular predictor
#' @param theta Paramter to model in radians
#' @param k degrees of freedom
#'
#' @return A harmonic function
#' @export
#'
#' @examples
#' theta = 2
#' k = 1
#' df <- pr_harmonic(theta = 2, k = 1)

pr_harmonic <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

#' Get coefficients from linear model
#'
#' @param df dataframe containing Year, Month, Parameters, values
#'
#' @return coefficients from linear model
#' @export
#'
#' @examples
#' df <- planktonr::pr_get_EOVs("NRS") %>%
#' dplyr::filter(StationCode %in% c('PHB', 'NSI'),
#' Parameters %in% c("Biomass_mgm3", "PhytoBiomassCarbon_pgL")) %>%
#'   pr_get_Coeffs()
pr_get_Coeffs <-  function(df){

  if(unique(df$Survey == "LTM")) {
    df <- df %>%
      dplyr::group_by(.data$StationCode, .data$StationName, .data$SampleTime_Local, .data$anomaly, .data$Year_Local, .data$Month_Local, .data$Parameters) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = "drop")
  }

  df <-  df %>%
    dplyr::rename(SampleDate = "SampleTime_Local") %>%
    dplyr::mutate(Month = .data$Month_Local * 2 * 3.142 / 12) %>%
    droplevels()

  params <- df %>%
    dplyr::select("Parameters") %>%
    unique()
  params <- params$Parameters

  if("BioRegion" %in% colnames(df)){
    stations <- df %>%
      dplyr::select("BioRegion") %>%
      unique()
    stations <- stations$BioRegion
  } else {
    stations <- df %>%
      dplyr::select("StationName") %>%
      unique()
    stations <- stations$StationName
  }

  params <- rep(params, each = length(stations))
  stations <- rep(stations, length.out = length(params))

  coeffs <- function(params, stations){
    lmdat <- df %>%
      dplyr::filter(.data$Parameters == params) %>%
      tidyr::drop_na()

    if("BioRegion" %in% colnames(df)){
      lmdat <- lmdat %>%
        dplyr::filter(.data$BioRegion == stations)
    } else {
      lmdat <- lmdat %>%
        dplyr::filter(.data$StationName == stations)
    }

    m <- stats::lm(Values ~ Year_Local + pr_harmonic(Month_Local, k = 1), data = lmdat)

    lmdat <- tibble::tibble(lmdat %>%
                              dplyr::bind_cols(fv = m$fitted.values))
    ms <- summary(m)
    slope <- ifelse(ms$coefficients[2,1] < 0, "decreasing", "increasing")
    p <- ifelse(ms$coefficients[2,4] < 0.005, "significantly", "but not significantly")

    if("StationName" %in% colnames(lmdat)) {
      dfs <- dplyr::tibble(slope = slope, p = p, Parameters = params, StationName = stations)
      dfs2 <- lmdat %>%
        dplyr::inner_join(dfs, by = c("Parameters", "StationName"))
    } else {
      dfs <- dplyr::tibble(slope = slope, p = p, Parameters = params, BioRegion = stations)
      dfs2 <- lmdat %>%
        dplyr::inner_join(dfs, by = c("Parameters", "BioRegion"))
    }
  }

  outputs <- purrr::map2(params, stations, coeffs) %>% purrr::list_rbind()

}


#' Return columns that are not taxonomic
#'
#' @param Survey `NRS` or `CPR`
#' @param Type `Z` or `P`
#'
#' @return a string vector of column name
#' @export
#'
#' @examples
#' str <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Z")
pr_get_NonTaxaColumns <- function(Survey = "NRS", Type = "Z"){

  if (Survey == "NRS" & Type == "Z"){
    vars <- c("Project", "StationName", "StationCode", "TripCode", "Latitude", "Longitude",
              "SampleTime_Local", "SampleTime_UTC", "Year_Local", "Month_Local", "Day_Local", "Time_Local24hr",
              "SampleDepth_m", "CTDSST_degC", "CTDChlaSurf_mgm3", "CTDSalinity_psu", "Biomass_mgm3", "AshFreeBiomass_mgm3")

  } else if (Survey == "NRS" & Type == "P"){
    vars <- c("Project", "StationName", "StationCode", "TripCode", "Latitude", "Longitude",
              "SampleTime_Local", "SampleTime_UTC", "Year_Local", "Month_Local", "Day_Local", "Time_Local24hr",
              "SampleDepth_m", "Method", "CTDSST_degC", "CTDChlaSurf_mgm3", "CTDSalinity_psu")

  } else if (Survey == "CPR" & Type == "Z"){
    vars <- c("TripCode", "Sample_ID", "Region", "Latitude", "Longitude", "SampleTime_UTC", "SampleTime_Local",
              "Year_Local", "Month_Local", "Day_Local", "Time_Local24hr", "SatSST_degC", "SatChlaSurf_mgm3",
              "PCI", "SampleVolume_m3", "BiomassIndex_mgm3")

  } else if (Survey == "CPR" & Type == "P"){
    vars <- c("TripCode", "Sample_ID", "Region", "Latitude", "Longitude", "SampleTime_Local", "SampleTime_UTC",
              "Year_Local", "Month_Local", "Day_Local", "Time_Local24hr", "SatSST_degC", "SatChlaSurf_mgm3",
              "PCI", "SampleVolume_m3")
  }

  return(vars)

}


#' Get species information table for Phytoplankton and Zooplankton
#'
#' @param Type Phytoplankton (P) or Zooplankton (Z)
#'
#' @return A dataframe of species information
#' @export
#'
#' @examples
#' dat <- pr_get_SpeciesInfo(Type = "P")
#' dat <- pr_get_SpeciesInfo(Type = "Z")
pr_get_SpeciesInfo <- function(Type = "Z"){

  if (Type == "P"){file = "phytoinfo"}
  if (Type == "Z"){file = "zoopinfo"}

  dat <- pr_get_s3(file) %>%
    pr_rename()

}




# Internal function to check Type
#
# @param Type Character string
#
# @return "Z" or "P"
#
# @examples
# pr_get_Type("phytoplankton")
# pr_get_Type("z")
# pr_get_Type <- function(Type){
#
#   zoop_type <- c("Z", "z", "zooplankton", "Zooplankton")
#   phyto_type <- c("P", "p", "phytoplankton", "Phytoplankton")
#
#   if(Type %in% zoop_type){
#     Type = "Z"
#   } else if (Type %in% phyto_type){
#     Type = "P"
#   }
#
# }


#' Helper function to reformat Titles
#'
#' @param tit String to reformat.
#'
#' @return A reformatted string
#' @export
#'
#' @examples
#' new_tit = pr_title("P")
pr_title <- function(tit){

  if (tit == "Z"){
    tit = "Zooplankton"
  }

  if (tit == "P"){
    tit = "Phytoplankton"
  }

  if (tit == "NRS"){
    tit = "National Reference Station"
  }

  if (tit == "CPR"){
    tit = "Continuous Plankton Recorder"
  }
  return(tit)

}



