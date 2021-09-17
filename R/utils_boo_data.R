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
pr_get_tsdata <- function(Survey = c("CPR", "NRS"), Type = c("P", "Z")){

  if(Type == "Z"){
    parameter1 <- "Biomass_mgm3"
    parameter2 <- "CopepodEvenness"
  } else
  {
    parameter1 <- "PhytoBiomassCarbon_pgL"
    parameter2 <- "DinoflagellateEvenness"
  }

    if(Survey == 'CPR'){
      dat <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "CPR_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
        dplyr::select(.data$Latitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$BioRegion, .data$Biomass_mgm3, .data[[parameter1]]:.data[[parameter2]]) %>%
        dplyr::mutate(Biomass_mgm3 = ifelse(.data$Biomass_mgm3 < 0 , 0, .data$Biomass_mgm3),
                      SampleDateUTC = lubridate::round_date(.data$SampleDateUTC, "month"),
                      YearMon = paste(.data$Year, .data$Month)) %>% # this step can be improved when nesting supports data pronouns
        tidyr::complete(.data$BioRegion, .data$YearMon) %>%
        dplyr::mutate(Year = as.numeric(stringr::str_sub(.data$YearMon, 1, 4)),
                      Month = as.numeric(stringr::str_sub(.data$YearMon, -2, -1))) %>%
        tidyr::pivot_longer(.data$Biomass_mgm3:.data$CopepodEvenness, values_to = "Values", names_to = 'parameters') %>%
        dplyr::group_by(.data$SampleDateUTC, .data$Year, .data$Month, .data$BioRegion, .data$parameters) %>%
        dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                         Latitude = mean(.data$Latitude, na.rm = TRUE), # so we can arrange by latitude and get the BioRegions in the correct order
                         .groups = "drop") %>%
        dplyr::filter(!is.na(.data$BioRegion),
                      .data$BioRegion != 'North',
                      .data$BioRegion != 'North-west') %>%
        dplyr::arrange(-.data$Latitude) # this is included to keep the mapping in the right order for the ui
      return(dat)
  } else
  {
    dat <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "NRS_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
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
