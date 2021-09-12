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

#' Plot basic timeseries
#'
#' @param df dataframe with SampleDateLocal, station code and parameters
#' @param pal is the palette name from cmocean
#'
#' @return a timeseries plot
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate ymd
#'
#' @examples
#' df <- data.frame(SampleDateLocal = c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18"), Code = 'NSI', Values = runif(4, min=0, max=10))
#' df <- df %>% mutate(SampleDateLocal = lubridate::ymd(SampleDateLocal))
#' timeseries <- pr_plot_timeseries(df, 'matter')
pr_plot_timeseries <- function(df, pal){
  n <- length(unique(df$Code))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)

  p1 <- ggplot(df, aes(x = .data$SampleDateLocal, y = Values)) +
    geom_line(aes(group = Code, color = Code)) +
    geom_point(aes(group = Code, color = Code)) +
    scale_x_datetime() +
    labs(y = "") +
    scale_colour_manual(values = plotCols) +
    theme(legend.position = "bottom")
  p1 <- ggplotly(p1) %>% layout(showlegend = FALSE)
  return(p1)
}


#' Plot single climatology
#'
#' @param df dataframe with specified time period, station code and parameter
#' @param x specified time period
#' @param pal is the palette name from cmocean
#'
#' @return a climatology plot
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), Code = 'NSI', Values = runif(120, min=0, max=10))
#' monthly <- pr_plot_climate(df, Month, 'matter')
pr_plot_climate <- function(df, x, pal){
  x <- dplyr::enquo(arg = x)

  n <- length(unique(df$Code))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)

  df_climate <- df %>% dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
    dplyr::group_by(!!x, .data$Code) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")

  p2 <- ggplot(df_climate, aes(x = !!x, y = .data$mean, fill = .data$Code)) +
    geom_col(position = position_dodge()) +
    geom_errorbar(aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se),
                  width = .2,                    # Width of the error bars
                  position = position_dodge(.9)) +
    labs(y = input$ycol) +
    scale_fill_manual(values = plotCols) +
    theme(legend.position = "bottom")

  p2 <- ggplotly(p2)
  return(p2)
}

