#' Access data for timeseries and climatology plots
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_tsdata()
pr_get_tsdata <- function(){
  datNRSi <- readr::read_csv(paste0(pr_get_outputs(), "NRS_Indices.csv"), na = "", show_col_types = FALSE) %>%
    mutate(Month = lubridate::month(SampleDateLocal),
           Year = lubridate::year(SampleDateLocal),
           Code = stringr::str_sub(TripCode, 1, 3),
           Name = stringr::str_c(Station, " (",Code,")"), # Create neat name for plotting
           Code = factor(Code),
           Name = factor(Name)) %>%
    tidyr::complete(Year, tidyr::nesting(Station, Code)) %>% # Turns implicit missing values into explicit missing values.
    dplyr::select(Year, Month, SampleDateLocal, Latitude, Station, Code, Biomass_mgm3:CopepodEvenness) %>%
    tidyr::pivot_longer(-c(Year:Code), values_to = 'Values', names_to = "parameters") %>%
    dplyr::arrange(-Latitude)  # Sort in ascending date order
    return(datNRSi)
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
