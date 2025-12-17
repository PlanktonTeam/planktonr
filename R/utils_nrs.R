#' Import NRS Station information
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_Stations()` was deprecated in planktonr 0.7.0. Station information
#' is now available through [pr_get_info()] with `Source = "NRS"` or
#' `Source = "SOTS"`.
#'
#' @param Survey Survey type: "NRS" or "SOTS"
#'
#' @return A dataframe with station information
#'
#' @seealso [pr_get_info()] for the preferred interface
#'
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_Stations('NRS')
#'
#' # Use:
#' dat <- pr_get_info(Source = "NRS")
#' }
#'
#' @importFrom rlang .data
pr_get_Stations <- function(Survey = 'NRS') {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_Stations()",
    with = "pr_get_info()"
  )
  pr_get_info(Source = Survey)
}


#' Filter dataframe on `StationCode` to return only NRS stations
#'
#'
#' @param dat Dataframe to filter
#'
#' @return Dataframe with only NRS Stations
#' @export
#'
#' @examples
#' \dontrun{dat <- pr_filter_NRSStations(dat)}
pr_filter_NRSStations <- function(dat){
  
  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame."
  )
  
  assertthat::assert_that(
    "StationCode" %in% colnames(dat),
    msg = "'dat' must contain a 'StationCode' column."
  )

  dat <- dat %>%
    dplyr::filter(.data$StationCode %in% c("DAR", "ESP", "KAI", "MAI", "NIN", "NSI", "PHB", "ROT", "VBM", "YON"))
}


