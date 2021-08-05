
# Testing
# devtools::document()
# pkgload::load_all()
# devtools::check()

## Functions for operating

#' Remove extra tibble info
#'
#' @param tib The tibble to convert to dataframe
#'
#' @return A dataframe with with all the tibble stuff removed
#' @export
#'
#' @examples
#' data(mtcars)
#' tib <- dplyr::as_tibble(mtcars)
#' df <- untibble(tib)
untibble <- function (tib) {
  tib <- data.frame(unclass(tib), check.names = FALSE, stringsAsFactors = FALSE)
  return(tib)
} ## escape the nonsense


#' Get location of raw plankton data
#'
#' Internal function to load the location of the raw plankton data files.
#' @return A string with location of raw plankton data
#' @export
#' @examples
#' file_loc <- get_raw_plankton()
#' @importFrom magrittr "%>%"
get_raw_plankton <- function(){

  raw <- "https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/"
  return(raw)
}


#' Load copepod information table with sizes etc.
#'
#' @return A dataframe with NRS zooplankton information
#' @export
#'
#' @examples
#' df <- get_ZooInfo()
#' @importFrom magrittr "%>%"
get_ZooInfo <- function(){
  ZInfo <- readr::read_csv(paste0(get_raw_plankton(), "ZoopInfo.csv"), na = "") %>%
    dplyr::rename( "TaxonName" = "TAXON_NAME") %>%
    untibble()
}
