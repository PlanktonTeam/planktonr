#' Access data for timeseries and climatology plots
#'
#' @param Survey CPR or NRS, defaults to NRS
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_tsdata("NRS", "Z")
pr_get_tsdata <- function(Survey = c('CPR', 'NRS'), Type = c("P", "Z")){

  if(Survey == 'CPR'){
    file <- "CPR_Indices.csv"
  } else
  {
    file <- "NRS_Indices.csv"
  }

  if(Type == "Z"){
    parameter1 <- "Biomass_mgm3"
    parameter2 <- "CopepodEvenness"
  } else
  {
    parameter1 <- "PhytoBiomassCarbon_pgL"
    parameter2 <- "DinoflagellateEvenness"
  }

  dat <- readr::read_csv(paste0(planktonr::pr_get_outputs(), file), na = "NA", show_col_types = FALSE) %>%
    dplyr::mutate(Month = lubridate::month(.data$SampleDateLocal),
                  Year = lubridate::year(.data$SampleDateLocal),
                  StatCode = paste(.data$StationName, .data$StationCode)) %>%
    dplyr::rename(Code = .data$StationCode) %>%
    #tidyr::complete(.data$Year, tidyr::nesting(Station, Code)) %>% # Nesting doesn't support data pronouns at this time
    tidyr::complete(.data$Year, .data$StatCode) %>%
    dplyr::mutate(Station = stringr::str_sub(.data$StatCode, 1, -5),
                  Code = stringr::str_sub(.data$StatCode, -3, -1)) %>%
    dplyr::select(.data$Year, .data$Month, .data$SampleDateLocal, .data$Latitude, .data$Station, .data$Code, .data[[parameter1]]:.data[[parameter2]]) %>%
    tidyr::pivot_longer(-c(.data$Year:.data$Code), values_to = 'Values', names_to = "parameters") %>%
    dplyr::arrange(-.data$Latitude)  # Sort in ascending date order
  return(dat)
}

#' To produce the climatology for plotting
#'
#' @param df output of pr_get_tsdata
#' @param x Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_climate functions
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), Code = 'NSI', Values = runif(120, min=0, max=10))
#' pr_make_climatology(df, Month)
#' @import dplyr
#' @importFrom stats sd
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_make_climatology <- function(df, x){
  x <- dplyr::enquo(arg = x)
  df_climate <- df %>% dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
    dplyr::group_by(!!x, .data$Code) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")
  return(df_climate)
}
