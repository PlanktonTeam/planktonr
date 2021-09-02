#' Get Larval Fish Trip Data
#'
#' @return A dataframe with Larval Fish Trip Data
#' @export
#'
#' @examples
#' df <- pr_get_LFTrips()
#' #' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFTrips <- function(){
  LFSamp <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_Samples.csv"), na = "", show_col_types = FALSE,
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    pr_rename() %>%
    pr_apply_time() %>%
    select(.data$i_Sample:.data$SampleDateLocal, .data$Year:.data$SampleDateLocal, .data$Latitude:.data$Comments)
}



#' Get Larval Fish Sample Data
#'
#' @return A dataframe with Raw Larval Fish Data
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
#' #' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_LFData <- function(){
  LFData <- readr::read_csv(paste0(pr_get_site(), "BGC_LFish_CountRaw.csv"), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Make count data of all larval fish
#'
#' @return A dataframe with Larval Fish Count Data
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
#' @return A dataframe with Larval Fish BGC Data
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
    select(-c(.data$ScientificName, .data$SPCode, .data$Temperature_degC, .data$Salinity_psu, .data$FlagComments)) %>%
    arrange(.data$Header) %>%
    tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
    arrange(.data$SampleDateLocal)
}
