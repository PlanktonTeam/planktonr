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

#' Sidebar panel plot of selected NRS stations
#'
#' @param df dataframe containing station codes to plot
#'
#' @return is a plotly map of the selected stations
#' @export
#'
#' @examples
#' df <- data.frame(StationCode = c("NSI", "PHB"))
#' pmap <- pr_plot_NRSmap(df)
pr_plot_NRSmap <-  function(df){
  meta2_sf <- subset(meta_sf, meta_sf$Code %in% df$StationCode)

  pmap <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::geom_sf(data = meta_sf, colour = "blue", size = 1.5) +
    ggplot2::geom_sf(data = meta2_sf, colour = "red", size = 1.5) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = NA, colour = NA),
          plot.background = ggplot2::element_rect(fill = NA),
          axis.line = ggplot2::element_blank())
  pmap <- plotly::ggplotly(pmap)
}

#' Title Sidebar panel plot of selected CPR bioregions
#'
#' @param df dataframe containing CPR bioregions to plot
#'
#' @return is a plotly map of the selected bioregions
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' df <- data.frame(BioRegion = c("Temperate East", "South-west"))
#' cprmap <- pr_plot_CPRmap(df)
pr_plot_CPRmap <-  function(df){
  bioregionSelection <- mbr %>% dplyr::filter(.data$REGION %in% df$BioRegion) %>%
      mutate(REGION = factor(.data$REGION, levels = c("Coral Sea", "Temperate East", "South-west", "South-east")))
  n <- length(unique(bioregionSelection$REGION))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mbr, colour = 'black', fill = 'white') +
    ggplot2::geom_sf(data = bioregionSelection, colour = 'black', ggplot2::aes(fill = .data$REGION)) +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::scale_fill_manual(values = cmocean::cmocean('matter')(n)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
          plot.background = ggplot2::element_rect(fill = NA),
          panel.background = ggplot2::element_rect(fill = NA),
          axis.line = ggplot2::element_blank())
}

#' Plot basic timeseries
#'
#' @param df dataframe with SampleDateLocal, station code and parameter name and values
#' @param pal is the palette name from cmocean
#' @param Survey CPR or NRS data
#' @param Scale scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a plotly timeseries plot
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 aes
#'
#' @examples
#' df <- data.frame(SampleDateLocal = c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18"),
#' StationCode = 'NSI', parameters = 'Biomass_mgm3', Values = runif(4, min=0, max=10),
#' Type = 'NRS')
#' df <- df %>% mutate(SampleDateLocal = as.POSIXct(paste(SampleDateLocal, "00:00:00"),
#' format = "%Y-%m-%d %H:%M:%S"))
#' timeseries <- pr_plot_timeseries(df, 'NRS', 'matter')
pr_plot_timeseries <- function(df, Survey = c("CPR", "NRS"), pal, Scale = 'identity'){
  if(Survey == 'CPR'){
    df <- df %>% dplyr::rename(SampleDate = .data$SampleDateUTC,
                               StationCode = .data$BioRegion)
    titlex <- 'Sample Date UTC'
  }
  if(Survey == 'NRS'){
    df <- df %>% dplyr::rename(SampleDate = .data$SampleDateLocal)
    titlex <- 'Sample Date Local'
  }

  n <- length(unique(df$StationCode))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)
  titley <- unique(df$parameters)
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleDate, y = .data$Values)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::geom_point(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::scale_x_datetime() +
    ggplot2::scale_y_continuous(trans = Scale) +
    ggplot2::labs(y = titley, x = titlex) +
    ggplot2::scale_colour_manual(values = plotCols)
  p1 <- plotly::ggplotly(p1) %>%
    plotly::layout(legend = list(orientation = "h", y = -0.1))
  p1
  return(p1)
}

#' Plot single climatology
#'
#' @param df dataframe with specified time period, station code and parameter
#' @param x specified time period
#' @param pal is the palette name from cmocean
#' @param Survey CPR or NRS data
#' @param Scale scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a plotly climatology plot
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom ggplot2 aes
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), StationCode = c('NSI', 'NSI', 'PHB', 'PHB'),
#' parameters = 'Biomass_mgm3', Values = runif(120, min=0, max=10))
#' monthly <- pr_plot_climate(df, "NRS", Month, 'matter')
pr_plot_climate <- function(df, Survey = c("CPR", "NRS"), x, pal, Scale = 'identity'){
  x <- dplyr::enquo(arg = x)

  if(Survey == 'CPR'){
    df <- df %>% dplyr::rename(StationCode = .data$BioRegion)
  }

  n <- length(unique(df$StationCode))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)
  title <- unique(df$parameters)
  df_climate <- df %>% dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, StationCode)
    dplyr::group_by(!!x, .data$StationCode) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")

  p2 <- ggplot2::ggplot(df_climate, ggplot2::aes(x = !!x, y = .data$mean, fill = .data$StationCode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se),
                  width = .2,                    # Width of the error bars
                  position = ggplot2::position_dodge(.9)) +
    ggplot2::labs(y = title) +
    ggplot2::scale_y_continuous(trans = Scale) +
    ggplot2::scale_fill_manual(values = plotCols)

  p2 <- plotly::ggplotly(p2) %>%
    plotly::layout(legend = list(orientation = "h", y = -0.1))
  return(p2)
}

#' Combined timeseries and climatology plots
#'
#' @param df data frame with SampleDateLocal, time period and parameter
#' @param pal is the palette name from cmocean
#' @param Survey CPR or NRS data
#' @param Scale scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return plotly combined plot
#' @export
#'
#' @examples
#' df <- data.frame(SampleDateLocal = c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18"),
#' Month = sample(1:12, 4), Year = 2012, StationCode = c('NSI', 'NSI', 'PHB', 'PHB'),
#' Values = runif(4, min=0, max=10))
#' df <- df %>% mutate(SampleDateLocal = as.POSIXct(paste(SampleDateLocal, "00:00:00"),
#' format = "%Y-%m-%d %H:%M:%S"))
#' monthly <- pr_plot_tsclimate(df, 'NRS', 'matter')
#' plotly::ggplotly(monthly)

pr_plot_tsclimate <- function(df, Survey = c("CPR", "NRS"), pal, Scale = 'identity'){

  p1 <- pr_plot_timeseries(df, Survey, pal, Scale) %>%
    plotly::layout(yaxis = list(title = ""))
  p2 <- pr_plot_climate(df, Survey, Month, pal, Scale)
  p3 <- pr_plot_climate(df, Survey, Year, pal, Scale) %>%
    plotly::layout(legend = list(orientation = "h", y = -0.1),
                   yaxis = list(title = ""))

  plots <- plotly::subplot(plotly::style(p1, showlegend = FALSE),
                           plotly::style(p2, showlegend = FALSE),
                           p3, nrows = 3, titleY = TRUE, titleX = TRUE,
                           margin = 0.05)
  return(plots)
}

