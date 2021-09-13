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
#' @importFrom ggplot2 aes
#'
#' @examples
#' df <- data.frame(SampleDateLocal = c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18"),
#' Code = 'NSI', Values = runif(4, min=0, max=10))
#' df <- df %>% mutate(SampleDateLocal = as.POSIXct(paste(SampleDateLocal, "00:00:00"),
#' format = "%Y-%m-%d %H:%M:%S"))
#' timeseries <- pr_plot_timeseries(df, 'matter')
pr_plot_timeseries <- function(df, pal){
  n <- length(unique(df$Code))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleDateLocal, y = .data$Values)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$Code, color = .data$Code)) +
    ggplot2::geom_point(ggplot2::aes(group = .data$Code, color = .data$Code)) +
    ggplot2::scale_x_datetime() +
    ggplot2::labs(y = "") +
    ggplot2::scale_colour_manual(values = plotCols) +
    ggplot2::theme(legend.position = "bottom")
  p1 <- plotly::ggplotly(p1)
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
#' @importFrom ggplot2 aes
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

  p2 <- ggplot2::ggplot(df_climate, ggplot2::aes(x = !!x, y = .data$mean, fill = .data$Code)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se),
                  width = .2,                    # Width of the error bars
                  position = ggplot2::position_dodge(.9)) +
    ggplot2::labs(y = "input$ycol") +
    ggplot2::scale_fill_manual(values = plotCols) +
    ggplot2::theme(legend.position = "bottom")

  p2 <- plotly::ggplotly(p2)
  return(p2)
}

