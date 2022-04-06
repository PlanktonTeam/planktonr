#' Get Larval Fish Trip Data
#'
#' @return A dataframe with Larval Fish Trip Data
#' @export
#'
#' @examples
#' df <- pr_get_LFTrips()
#' #' @import dplyr
#' @importFrom rlang .data
pr_get_LFTrips <- function(){

  LFSamp <- readr::read_csv(system.file("extdata", "BGC_LFish_Samples.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE,
                            col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
    pr_rename() %>%
    pr_apply_time() %>%
    dplyr::select(.data$i_Sample:.data$SampleDate_Local, .data$Year:.data$SampleDate_Local, .data$Latitude:.data$Comments)
}



#' Get Larval Fish Sample Data
#'
#' @return A dataframe with Raw Larval Fish Data
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
#' #' @import dplyr
#' @importFrom rlang .data
pr_get_LFData <- function(){
  LFData <- readr::read_csv(system.file("extdata", "BGC_LFish_CountRaw.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE) %>%
    pr_rename()
}


#' Make count data of all larval fish
#'
#' @return A dataframe with Larval Fish Count Data
#' @export
#'
#' @examples
#' df <- pr_get_LFCountAll()
#' @importFrom rlang .data
pr_get_LFCountAll <- function(){

  LFCount <- pr_get_LFTrips() %>%
    dplyr::left_join(pr_get_LFData() %>% dplyr::select(-.data$Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
    dplyr::select(-.data$ScientificName, -.data$SPCode) %>%
    dplyr::arrange(.data$Header) %>%
    tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
    dplyr::arrange(.data$SampleDate_Local)
}



#' Make BGC data for larval fish
#'
#' @return A dataframe with Larval Fish BGC Data
#' @export
#'
#' @examples
#' df <- pr_get_LFCountBGC()
#' @importFrom rlang .data
pr_get_LFCountBGC <- function(){
  LFCountBGC <- pr_get_LFTrips() %>%
    dplyr::filter(grepl('IMOS', .data$ProjectName)) %>%
    dplyr::left_join(pr_get_LFData() %>%
                       dplyr::select(-.data$Comments), by = c("i_Sample", "TripCode")) %>%
    dplyr::mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
    dplyr::select(-c(.data$ScientificName, .data$SPCode, .data$Temperature_degC, .data$Salinity_psu, .data$FlagComments)) %>%
    dplyr::arrange(.data$Header) %>%
    tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
    dplyr::arrange(.data$SampleDate_Local)
}
