#' Get Larval Fish Trip Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFTrips()
#' 
pr_get_LFTrips <- function(){
  LFSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_Samples.csv"), na = "",
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    pr_rename() %>%
    mutate(Year = lubridate::year(SampleDateLocal),
                  Month = lubridate::month(SampleDateLocal),
                  Day = lubridate::day(SampleDateLocal),
                  Time_24hr = stringr::str_sub(SampleDateLocal, -8, -1), # hms doesn"t seem to work on 00:00:00 times
                  tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast", warn = FALSE),
                  SampleDateUTC = lubridate::with_tz(lubridate::force_tzs(SampleDateLocal, tz, roll = TRUE), "UTC")) %>%
    select(i_Sample:SampleDateLocal, Year:SampleDateLocal, Latitude:Comments)
}



#' Get Larval Fish Sample Data
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
#' 
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

pr_get_LFCountAll <- function(){

  LFCount <- pr_get_LFTrips() %>%
    left_join(pr_get_LFData() %>% select(-Comments), by = c("i_Sample", "TripCode")) %>%
    mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    select(-ScientificName, -SPCode) %>%
    arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = TaxonCount, values_fill = 0) %>%
    arrange(SampleDateLocal)
}



#' Make BGC data for larval fish
#'
#' @return
#' @export
#'
#' @examples
#' df <- pr_get_LFCountBGC()

pr_get_LFCountBGC <- function(){
  LFCountBGC <- pr_get_LFTrips() %>%
    filter(grepl('IMOS', ProjectName)) %>%
    left_join(pr_get_LFData() %>%
                       select(-Comments), by = c("i_Sample", "TripCode")) %>%
    mutate(Header = paste(ScientificName, SPCode, sep = " ")) %>%
    select(-c(ScientificName, SPCode, Temperature_degC, Salinity_psu)) %>%
    arrange(Header) %>%
    tidyr::pivot_wider(names_from = Header, values_from = TaxonCount, values_fill = 0) %>%
    arrange(SampleDateLocal)
}
