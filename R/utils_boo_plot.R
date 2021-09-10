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
#' @param df dataframe with SampleDateLocal and parameters
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
#' plotCols <- cmocean::cmocean('matter')(1)
#' timeseries <- pr_plot_timeseries(df)
pr_plot_timeseries <- function(df, plotCols){
  p1 <- ggplot(df, aes(x = .data$SampleDateLocal, y = Values)) +
    geom_line(aes(group = Code, color = Code)) +
    geom_point(aes(group = Code, color = Code)) +
    scale_x_datetime() +
    labs(y = "") +
    scale_colour_manual(values = plotCols) +
    theme(legend.position = "none")
  p1 <- ggplotly(p1) %>% layout(showlegend = FALSE)
  return(p1)
}
