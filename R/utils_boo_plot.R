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
pr_get_PlotCols <- function(pal = 'matter', n){
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
#' Survey = 'NRS')
#' df <- df %>% mutate(SampleDateLocal = as.POSIXct(paste(SampleDateLocal, "00:00:00"),
#' format = "%Y-%m-%d %H:%M:%S"))
#' timeseries <- pr_plot_timeseries(df, 'NRS', 'matter')
pr_plot_timeseries <- function(df, Survey = "NRS", pal = 'matter', Scale = 'identity'){

  if(Survey == 'CPR'){
    df <- df %>%
      dplyr::rename(SampleDate = .data$SampleDateUTC,
                    StationCode = .data$BioRegion)
    titlex <- 'Sample Date (UTC)'
  }

  if(Survey == 'NRS'){
    df <- df %>%
      dplyr::rename(SampleDate = .data$SampleDateLocal) %>%
      dplyr::group_by(.data$SampleDate, .data$StationCode, .data$parameters) %>% # accounting for microbial data different depths
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = 'drop')
    titlex <- 'Sample Date (Local)'
  }

  n <- length(unique(df$StationCode))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)
  titley <- planktonr::pr_relabel(unique(df$parameters), style = "ggplot")

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleDate, y = .data$Values)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::geom_point(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(trans = Scale) +
    ggplot2::labs(y = titley,
                  x = titlex) +
    ggplot2::scale_colour_manual(values = plotCols) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = 'bottom')

  return(p1)
}


#' Plot temporal trends in plankton data
#'
#' @param df A dataframe containing the plankton timeseries data.
#' @param trend Over what timescale to fit the trend - "Raw", "Month" or "Year"
#' @param survey "NRS" or "CPR" data
#' @param method Any method accepted by `geom_smooth()`
#' @param pal is the palette name from `cmocean()`
#' @param y_trans transformation of y axis on plot, whatever `scale_y_continuous()` trans accepts
#' @param output is the plot style - "ggplot" or "plotly"
#' #'
#' @return a timeseries plot
#'
#' @importFrom rlang "!!"
#'
#' @export
#'
#' @examples
#' df <- pr_get_tsdata("NRS", "Z") %>% filter(parameters == 'Biomass_mgm3')
#' plot <- pr_plot_trends(df, survey = "NRS")
pr_plot_trends <- function(df, trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = "identity", output = "ggplot"){

  if (survey == "CPR"){
    time = rlang::sym("SampleDateUTC")
    site = rlang::sym("BioRegion")
  } else if (survey == "NRS"){
    time = rlang::sym("SampleDateLocal")
    site = rlang::sym("StationName")
  }

  titley <- planktonr::pr_relabel(unique(df$parameters), style = output)

  # Averaging based on `trend` ----------------------------------------------

  if (trend %in% c("Year", "Month")){
    df <- df %>%
      dplyr::filter(!is.na(!!time))  %>% # need to drop NA from month, added to dataset by complete(Year, Code)
      dplyr::mutate(Year = lubridate::year(!!time), # I don't need both each time but probably quicker to just do it
                    Month = lubridate::month(!!time)) %>%
      dplyr::group_by(!!rlang::sym(trend), !!site) %>%
      dplyr::summarise(value = mean(.data$Values, na.rm = TRUE),
                       N = dplyr::n(),
                       sd = sd(.data$Values, na.rm = TRUE),
                       se = sd / sqrt(N),
                       .groups = "drop")

  } else {
    trend <- time # Rename trend to match the column with time
    df <- df %>%
      dplyr::group_by(!!time, !!site, .data$parameters) %>% # accounting for microbial data different depths
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = 'drop')%>%
      rename(value = Values)
  }

  # Do the plotting ---------------------------------------------------------

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(trend), y = .data$value)) + # do this logging as in pr_plot_tsclimate
    ggplot2::geom_smooth(method = method, formula = y ~ x) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(rlang::enexpr(site), scales = "free_y", ncol = 1) +
    ggplot2::ylab(rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = y_trans) +
    ggplot2::theme(legend.position = "bottom",
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(trend) %in% c("Month")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(trend) %in% c("Year")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2) +
      ggplot2::xlab("Year")
  } else if (!rlang::as_string(trend) %in% c("Month", "Year")){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::xlab("Year")
  }

  if (output %in% "plotly"){
    p1 <- plotly::ggplotly(p1)
  }


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
pr_plot_climate <- function(df, Survey = "NRS", x, pal = 'matter', Scale = 'identity'){

  x <- dplyr::enquo(arg = x)

  if(Survey == 'CPR'){
    df <- df %>%
      dplyr::rename(StationCode = .data$BioRegion)
  }

  n <- length(unique(df$StationCode))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)
  title <- planktonr::pr_relabel(unique(df$parameters), style = "ggplot")

  df_climate <- df %>%
    dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, StationCode)
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
    ggplot2::scale_fill_manual(values = plotCols)  +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = 'bottom')

  if("Month" %in% colnames(df_climate)){
    p2 <- p2 +
      ggplot2::scale_x_continuous(breaks = seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  }
  if("Year" %in% colnames(df_climate)){
    p2 <- p2 +
      ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))
  }

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
#' Month = sample(1:12, 4), Year = c(2012, 2013, 2014, 2015),
#' StationCode = c('NSI', 'NSI', 'PHB', 'PHB'),
#' parameters = 'Biomass_mgm3',
#' Values = runif(4, min=0, max=10))
#' df <- df %>% mutate(SampleDateLocal = as.POSIXct(paste(SampleDateLocal, "00:00:00"),
#' format = "%Y-%m-%d %H:%M:%S"))
#' monthly <- pr_plot_tsclimate(df, 'NRS', 'matter')


pr_plot_tsclimate <- function(df, Survey = c("CPR", "NRS"), pal = 'matter', Scale = 'identity'){

  p1 <- pr_plot_timeseries(df, Survey, pal, Scale) + ggplot2::theme(legend.position = 'none',
                                                             axis.title.y = ggplot2::element_blank())

  p2 <- pr_plot_climate(df, Survey, .data$Month, pal, Scale) + ggplot2::theme(legend.position = 'none')

  p3 <- pr_plot_climate(df, Survey, .data$Year, pal, Scale) + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                      legend.title = ggplot2::element_blank())

  plots <- patchwork::wrap_plots(p1, p2,  p3, nrow = 3)

  return(plots)
}

#' Time series plot of functional groups
#'
#' @param df dataframe in format of output from pr_get_fg
#' @param Scale y axis scale Actual or Percent
#' @param trend Over what timescale to fit the trend - "Raw", "Month" or "Year"
#'
#' @return plot of fg timseries
#' @export
#'
#' @examples
#' df <- pr_get_fg('NRS', 'P')
#' plot <- pr_plot_tsfg(df, 'Actual')
pr_plot_tsfg <- function(df, Scale = 'Actual', trend = 'Raw'){
  titley <- planktonr::pr_relabel("FunctionalGroup", style = "ggplot")
  n <- length(unique(df$parameters))
  plotCols <- planktonr::pr_get_PlotCols('matter', n)

 if("SampleDateUTC" %in% colnames(df)){
    SampleDate = rlang::sym("SampleDateUTC")
    station = rlang::sym("BioRegion")
    titlex <- 'Sample Date UTC'
  } else {
    SampleDate = rlang::sym("SampleDateLocal")
    station = rlang::sym("StationName")
    titlex <- 'Sample Date Local'
  }

  if (trend %in% c("Year", "Month")){
    df <- df %>%
      dplyr::filter(!is.na(!!SampleDate))  %>%
      dplyr::group_by(!!rlang::sym(trend), !!station, .data$parameters) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       N = dplyr::n(),
                       sd = sd(.data$Values, na.rm = TRUE),
                       se = sd / sqrt(.data$N),
                       .groups = "drop")

  } else {
    trend <- SampleDate # Rename trend to match the column with time
  }

 if(Scale == 'Percent') {
    df <- df %>%
      dplyr::group_by(!!rlang::sym(trend), !!station, .data$parameters) %>%
      dplyr::summarise(n = sum(.data$Values, na.rm = TRUE)) %>%
      dplyr::mutate(Values = .data$n / sum(.data$n, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else {
    df <- df %>% mutate(Values = log10(.data$Values))
  }

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(trend), y = .data$Values, fill = .data$parameters)) +
    ggplot2::geom_area(alpha=0.6 , size=1, colour="white") +
    ggplot2::facet_wrap(rlang::enexpr(station), scales = "free", ncol = 1) +
    ggplot2::labs(y = titley) +
    ggplot2::scale_fill_manual(values = plotCols) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(trend) %in% c("Month")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(trend) %in% c("Year")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2) +
      ggplot2::xlab("Year")
  } else if (rlang::as_string(trend) %in% c("SampleDate")){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::xlab("Sample Date")
  }
  return(p1)
  }

#' Policy plot
#'
#' @param df dataframe containing timeseries data with parameters and Values
#' @param EOV Essential OCean Variable as a parameter
#' @param Survey NRS, CPR or Long term monitoring
#' @param trans scale for y axis
#' @param pal colour pallet from cmocean
#' @param labels do you want to print labels on the x axes
#'
#' @return plot of timeseries, anomalies and climatology
#' @export
#'
#' @examples
#' df <- data.frame(SampleDate = c("2010-01-01","2010-02-25","2010-06-21","2010-04-11","2010-08-05"),
#' StationName = 'Port Hacking', parameters = 'Biomass_mgm3', Values = runif(5, 1, 50),
#' fv = runif(5, 1, 50), anomaly = runif(5, 1, 3), Month = runif(5, 1, 6)) %>%
#' dplyr::mutate(SampleDate = as.POSIXct(SampleDate))
#' plot <-  pr_plot_EOV(df, 'Biomass_mgm3', 'NRS', 'identity', 'matter', 'yes')
pr_plot_EOV <- function(df, EOV = "Biomass_mgm3", Survey = 'NRS', trans = 'identity', pal = 'matter', labels = "yes") {

  titley <- planktonr::pr_relabel(EOV, style = "ggplot")

  pals <- planktonr::pr_get_PlotCols(pal = pal, n = 20)
  col <- pals[15]
  colin <- pals[5]
  if(Survey == "LTM"){
    lims <- as.POSIXct(strptime(c("1944-01-01","2020-31-31"), format = "%Y-%m-%d"))
    df <- df %>% dplyr::filter(.data$parameters == EOV)
  } else {
    lims <- as.POSIXct(strptime(c("2010-01-01","2020-31-31"), format = "%Y-%m-%d"))
    df <- df %>% dplyr::filter(.data$parameters == EOV) %>% dplyr::rename(SampleDate = 1)
  }

  p1 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$SampleDate, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$SampleDate, y = .data$fv), method = "lm", formula = 'y ~ x', colour = col, fill = colin) +
    ggplot2::labs(x = "Year", y = rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::theme(legend.position = "none")

  if(labels == "no"){
    p1 <- p1 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if(Survey == 'LTM'){
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims)
  } else {
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims)
  }

  p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleDate, .data$anomaly)) +
    ggplot2::geom_col(fill = colin, colour = col) +
    ggplot2::xlab("Year") +
    ggplot2::labs(y = "Anomaly")

  if(labels == "no"){
    p2 <- p2 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if(Survey == 'LTM'){
    p2 <-  p2 + ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims)
  } else {
    p2 <-  p2 + ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims)
  }

  p3 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$Month, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$Month, y = .data$fv), method = "loess", formula = 'y ~ x', colour = col, fill = colin) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::scale_x_continuous(breaks = seq(0.5, 6.3, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::xlab("Month") +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  if(labels == "no"){
    p3 <- p3 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  p1 + p2 + p3 + patchwork::plot_layout(widths = c(3,3,2))

}



#' Combined timeseries and climatology plots for environmental variables
#'
#' @param df A dataframe from pr_get_nuts or pr_get_pigs
#' @param pal A Palette from cmocean
#' @param trend Trend line to be used, options None, Smoother, Linear
#' @param Scale scale on which to plot y axis
#'
#' @return A plotly plot with timeseries and climatology at depths
#' @export
#'
#' @examples
#' df <- pr_get_nuts() %>% pr_plot_env_var()
pr_plot_env_var <- function(df, pal = 'matter', trend = 'None', Scale = 'identity') {
  n <- length(unique(df$StationName))
  plotCols <- planktonr::pr_get_PlotCols(pal, n)
  # titley <- unique(df$parameters)
  titley <- planktonr::pr_relabel(unique(df$parameters), style = "plotly")
  np <- length(unique(df$SampleDepth_m))

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleDateLocal, .data$Values, colour = .data$StationName)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = titley) +
    ggplot2::facet_grid(SampleDepth_m ~., scales = "free") +
    ggplot2::theme_bw() + ggplot2::theme(strip.background = ggplot2::element_blank(),
                                         strip.text = ggplot2::element_blank(),
                                         legend.position = "bottom",
                                         legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(trans = Scale) +
    ggplot2::scale_colour_manual(values = plotCols)

  if(trend == "Smoother"){
    p <- p + ggplot2::geom_smooth(method = 'loess', formula = y ~ x)
  }
  if(trend == "Linear"){
    p <- p + ggplot2::geom_smooth(method = 'lm', formula = y ~ x)
  }

  p <- plotly::ggplotly(p, height = 150 * np)

  mdat <- df %>% group_by(.data$StationName, .data$Month, .data$SampleDepth_m, .data$parameters) %>%
    summarise(MonValues = mean(.data$Values, na.rm = TRUE),
              N = length(.data$Values),
              sd = stats::sd(.data$Values, na.rm = TRUE),
              se = sd / sqrt(.data$N),
              .groups = 'drop')

  m <- ggplot2::ggplot(mdat, aes(.data$Month, .data$MonValues, colour = .data$StationName)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(.data$SampleDepth_m ~., scales = "free") +
    ggplot2::geom_smooth(method = 'loess', formula = y ~ x) +
    ggplot2::scale_x_continuous(breaks= seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::scale_colour_manual(values = plotCols) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(face = "bold", size = 12, angle = 0))

  m <- plotly::ggplotly(m) %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                            x = 0.5, y = -0.1))

  plot <- plotly::subplot(plotly::style(p, showlegend = FALSE), m, widths = c(0.75,0.25)) %>%
    plotly::layout(title = list(text = titley),
                   annotations = list( x = 0.97, y = 1.0, text = "Depth (m)", xref = "paper", yref = "paper",
                                       xanchor = "center", yanchor = "bottom", showarrow = FALSE))
}

#' Frequency plot of the selected species
#'
#' @param df dataframe of format similar to output of pr_get_fmap_data()
#'
#' @return a plot of frequency of occurence of chosen species
#' @export
#'
#' @examples
#' df <- data.frame(Long = c(110, 130, 155, 150), Lat = c(-10, -35, -27, -45),
#' freqfac = c("Absent", "Seen in 25%",'50%', '75%'),
#' Season = c("December - February","March - May","June - August","September - November"),
#' Taxon = 'Acartia danae')
#' plot <- pr_plot_fmap(df)
pr_plot_fmap <- function(df){
  cols <- c("lightblue1" ,"skyblue3", "dodgerblue2","blue1", "navyblue")

  Species <- unique(df$Taxon)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = MapOz) +
    ggplot2::geom_point(data=df, ggplot2::aes(x=.data$Long, y=.data$Lat, colour=.data$freqfac), size = 2) +
    ggplot2::facet_wrap( ~ .data$Season, dir = "v") +
    ggplot2::labs(title = Species) +
    ggplot2::scale_colour_manual(name = '', values = cols, drop = FALSE) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   title = ggplot2::element_text(face = "italic"),
                   legend.title = ggplot2::element_text(face = "plain", size = 12),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = 'snow1'),
                   legend.position = 'bottom',
                   legend.key = ggplot2::element_blank())

}


#' Plot of relative day and night abundances
#'
#' @param df dataframe as output of pr_get_daynight() filtered for one species
#'
#' @return plot of relative day and night abundances
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(seq(1,12,1),2), daynight = c(rep('day', 12), rep('night', 12)),
#' CopeAbundance_m3 = runif(24, 0.1, 10), Species = 'Acartia danae')
#' plot <- pr_plot_daynight(df)
pr_plot_daynight <-  function(df){

  titlemain <- unique(df$Species)
  if("CopeAbundance_m3" %in% names(df)){
    ylabel <- planktonr::pr_relabel("CopeAbundance_m3", style = "ggplot") # this is probably only worth doing for copepods as we don't have a lot of data for other things
  } else {
    ylabel <- planktonr::pr_relabel("PhytoAbund_m3", style = "ggplot")
  }

  plots <- ggplot2::ggplot(df, ggplot2::aes(.data$Month, .data$Species_m3)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(formula = 'y ~ x', method = 'loess') +
    ggplot2::facet_grid(~ .data$daynight, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks= seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = ylabel) +
    ggplot2::ggtitle(titlemain) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = 'italic'))

}


#' Plot of STI kernel density for species
#'
#' @param df dataframe as output of pr_get_sti() filtered for one species
#'
#' @return plot of STI kernel density
#' @export
#'
#' @examples
#' df <- data.frame(sst = runif(24, 5, 25), Project = c(rep('cpr', 12), rep('nrs', 12)),
#' Species_m3 = runif(24, 0.1, 10), Species = 'Acartia danae')
#' plot <- pr_plot_sti(df)
pr_plot_sti <-  function(df){
  means <- df %>% dplyr::group_by(.data$Project) %>%
    dplyr::summarise(mean = mean(.data$Species_m3, na.rm = TRUE))

  #means are so different so log data as the abundance scale is so wide

  sti <- df %>% dplyr::left_join(means, by = 'Project') %>%
    dplyr::mutate(relab = .data$Species_m3/.data$mean) %>%
    dplyr::group_by(.data$sst, .data$Species) %>%
    dplyr::summarize(relab = sum(.data$relab),
                     freq = n(),
                     a = sum(.data$relab)/n(),
                     .groups = 'drop')

  n <- length(sti$sst)
  # have a stop if n < 10

  ## STI via kernel density

  kernStep <- 0.001
  kernMin <- 0
  kernMax <- 32
  kernN <- round((kernMax - kernMin) / kernStep + 1)
  kernTemps <- seq(kernMin, kernMax, length.out=kernN)
  kernBw <- 2

  kern_yp <- matrix(0, nrow = kernN, ncol = 1)
  kypout <- matrix(0, nrow = kernN, ncol = 1)

  taxon <- unique(sti$Species)
  sti$Species <- factor(sti$Species)
  sti$weight <- with(sti, abs(relab) / sum(relab))
  kernOut <- with(sti,
                  density(sst, weight=weight,
                          bw=kernBw,
                          from=kernMin,
                          to=kernMax,
                          n=kernN))

  z <- data.frame(kernTemps, y = kernOut$y)
  STI <- round(z[which.max(z[,2]),]$kernTemps,1)

# looks at these lines if we want to plot on an abundance scale instead of 0 - 1 scale
#  kern_yp[,i] <- kernOut$y/sum(kernOut$y) * 100 * mean(sti$relab)
#  kypout[,i] <- kernOut$y

  xlabel <- planktonr::pr_relabel("Temperature_degC", style = "ggplot")
  subtit <- rlang::expr(paste("STI = ",!!STI,degree,"C"))

  stiplot <- ggplot2::ggplot(z, aes(kernTemps, .data$y)) + ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = STI, colour = 'blue', lty = 4) +
    ggplot2::theme_bw() +
    ggplot2::labs(x=xlabel, y="Relative kernel density") +
    ggplot2::ggtitle(taxon, subtitle = subtit) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = 'italic'))

}


