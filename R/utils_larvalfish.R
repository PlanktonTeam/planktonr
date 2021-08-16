#' Get Larval Fish Trip Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFTrips()
#' #' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFTrips <- function(){
  LFSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_Samples.csv"), na = "",
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    pr_rename() %>%
    # rename(.data$i_Sample = I_SAMPLE_ID, .data$TripCode = TRIP_CODE, Station = STATIONNAME,
    #               .data$Latitude = LATITUDE, .data$Longitude = LONGITUDE, .data$SampleDateLocal = SAMPLEDATELOCAL,
    #               .data$ProjectName = PROJECTNAME, .data$Volume_m3 = VOLUME_M3, .data$Vessel = VESSEL,
    #               .data$TowType = TOWTYPE, .data$GearDepth_m = .data$GearDepth_M, .data$GearMesh_um = GEARMESH_UM,
    #               .data$WaterDepth_m = BATHYM_M, Temp_DegC = TEMPERATURE_C, Salinity = SALINITY,
    #               .data$Comments = .data$Comments, QC_Flag = QC_Flag, Flag.data$Comments = FLAG_COMMENT) %>%
    mutate(Year = lubridate::year(.data$SampleDateLocal),
                  Month = lubridate::month(.data$SampleDateLocal),
                  Day = lubridate::day(.data$SampleDateLocal),
                  Time_24hr = stringr::str_sub(.data$SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(.data$SampleDateLocal, .data$tz, roll = TRUE), "UTC")) %>%
    select(.data$i_Sample:.data$SampleDateLocal, .data$Year:.data$SampleDateLocal, .data$Latitude:.data$Comments)
}



#' Get Larval Fish Sample Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
#' #' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFData <- function(){
  LFData <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_CountRaw.csv"), na = "") %>%
    pr_rename()
}


#' Make count data of all larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFCountAll()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFCountAll <- function(){

  LFCount <- pr_get_LFTrips() %>%
    left_join(pr_get_LFData() %>% select(-.data$Comments), by = c("i_Sample", "TripCode")) %>%
    mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
    select(-.data$ScientificName, -.data$SPCode) %>%
    arrange(.data$Header) %>%
    tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
    arrange(.data$SampleDateLocal)
}



#' Make BGC data for larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFCountBGC()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFCountBGC <- function(){
  LFCountBGC <- pr_get_LFTrips() %>%
    filter(grepl('IMOS', .data$ProjectName)) %>%
    left_join(pr_get_LFData() %>%
                       select(-.data$Comments), by = c("i_Sample", "TripCode")) %>%
    mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
    select(-c(.data$ScientificName, .data$SPCode, .data$Temperature_degC, .data$Salinity_psu)) %>%
    arrange(.data$Header) %>%
    tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
    arrange(.data$SampleDateLocal)
}
