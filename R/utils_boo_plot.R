#' Setting the colours for plots using cmocean
#'
#' @param pal is the palette name
#' @param n is the number of colours required
#'
#' @return is a list of colours of length n from palette pal
#' @export
#'
#' @importFrom cmocean cmocean
#'
#' @examples
#' plotCols <- pr_get_PlotCols('matter', 5)
pr_get_PlotCols <- function(pal, n){
  plotCols <- cmocean::cmocean(pal)(n)
  return(plotCols)
}


#' To produce the base map of Australia for plotting
#'
#' @return an sf object for plotting the base map of Australia
#' @export
#'
#' @import sf
#'
#' @examples
#' MapOz <- pr_get_MapOz()
pr_get_MapOz <- function(){
  MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                       returnclass = "sf")
  return(MapOz)
}

#' To produce the climatology for plotting
#'
#' @param df data frame containing columns Year, Month, Day
#' @param x Year, Month, Day, time period of climatology
#'
#' @return a dataframe for plotting
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
