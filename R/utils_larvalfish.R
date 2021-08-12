#' Get Larval Fish Trip Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFTrips()
#' #' @importFrom magrittr "%>%"
pr_get_LFTrips <- function(){
  LFSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_Samples.csv"), na = "",
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    dplyr::rename(i_Sample = I_SAMPLE_ID, TripCode = TRIP_CODE, Station = STATIONNAME,
                  Latitude = LATITUDE, Longitude = LONGITUDE, SampleDateLocal = SAMPLEDATELOCAL,
                  ProjectName = PROJECTNAME, Volume_m3 = VOLUME_M3, Vessel = VESSEL,
                  TowType = TOWTYPE, GearDepth_m = GEARDEPTH_M, GearMesh_um = GEARMESH_UM,
                  WaterDepth_m = BATHYM_M, Temp_DegC = TEMPERATURE_C, Salinity = SALINITY,
                  Comments = COMMENTS, QC_Flag = QC_FLAG, FlagComments = FLAG_COMMENT) %>%
    dplyr::mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    dplyr::select(i_Sample:SampleDateLocal, Year:SampleDateLocal, Latitude:FlagComments)
}



#' Get Larval Fish Sample Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
#' #' @importFrom magrittr "%>%"
pr_get_LFData <- function(){
  LFData <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_CountRaw.csv"), na = "") %>%
    dplyr::rename(i_Sample = I_SAMPLE_ID, TripCode = TRIP_CODE,
                  ScientificName = SCIENTIFICNAME, SPCode = SPCode,
                  Taxon_Count = TAXON_COUNT, Comments = COMMENTS)
}


#' Make count data of all larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFCountAll()
#' @importFrom magrittr "%>%"
pr_get_LFCountAll <- function(){

  LFCount <- pr_get_LFTrips() %>%
    dplyr::left_join(pr_get_LFData() %>%
                       dplyr::select(-Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    dplyr::select(-ScientificName, -SPCode) %>%
    dplyr::arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = Taxon_Count, values_fill = 0) %>%
    dplyr::arrange(SampleDateLocal)
}



#' Make BGC data for larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFCountBGC()
#' @importFrom magrittr "%>%"
pr_get_LFCountBGC <- function(){
  LFCountBGC <- pr_get_LFTrips() %>%
    dplyr::filter(grepl('IMOS', ProjectName)) %>%
    dplyr::left_join(pr_get_LFData() %>%
                       dplyr::select(-Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    dplyr::select(-c(ScientificName, SPCode, Temp_DegC, Salinity)) %>%
    dplyr::arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = Taxon_Count, values_fill = 0) %>%
    dplyr::arrange(SampleDateLocal)
}
