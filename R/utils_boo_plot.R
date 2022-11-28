
#' Sidebar panel plot of selected NRS stations
#'
#' @param df dataframe containing station codes to plot
#'
#' @return a map of the selected stations
#' @export
#'
#' @examples
#' df <- data.frame(StationCode = c("NSI", "PHB"))
#' pmap <- pr_plot_NRSmap(df)
pr_plot_NRSmap <- function(df){

  meta_sf <- meta_sf %>%
    dplyr::mutate(Colour = dplyr::if_else(.data$Code %in% df$StationCode, "Red", "Blue")) %>%
    sf::st_as_sf() # This seems to strip away some of the tibble stuff that makes the filter not work...
  # dplyr::filter(.data$Code %in% df$StationCode)

  col <- meta_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select("Code", "Colour") %>%
    tibble::deframe()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::geom_sf(data = meta_sf, ggplot2::aes(colour = .data$Code), size = 5, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = col) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))

  return(p1)
}

#' Title Sidebar panel plot of selected CPR bioregions
#'
#' @param df dataframe containing CPR bioregions to plot
#'
#' @return a map of the selected bioregions
#' @export
#'
#'
#' @examples
#' df <- data.frame(BioRegion = c("Temperate East", "South-west", "South-east", "North", "North-west"))
#' cprmap <- pr_plot_CPRmap(df)
pr_plot_CPRmap <-  function(df){

  bioregionSelection <- mbr %>%
    dplyr::mutate(Colour = dplyr::if_else(.data$REGION %in% df$BioRegion, .data$Colour, "NA"))

  col <- bioregionSelection %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct() %>%
    dplyr::select("REGION", "Colour") %>%
    tibble::deframe()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = bioregionSelection, colour = "black", ggplot2::aes(fill = .data$REGION)) +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   axis.line = ggplot2::element_blank())

  return(p1)
}

#' Plot basic timeseries
#'
#' @param df dataframe with SampleDate_Local, station code and parameter name and values
#' @param Survey CPR or NRS data
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a timeseries plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "Z") %>%
#'   dplyr::filter(Parameters == "Biomass_mgm3")
#' timeseries <- pr_plot_TimeSeries(df, "NRS")
pr_plot_TimeSeries <- function(df, Survey = "NRS", trans = "identity"){

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::rename(StationCode = "BioRegion")
    plotCols <- colCPR

  }

  if(Survey == "NRS"){

    df <- df %>%
      dplyr::group_by(.data$SampleTime_Local, .data$StationCode, .data$Parameters) %>% # accounting for microbial data different depths
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = "drop")
    plotCols <- colNRS
  }

  titlex <- "Sample Date (Local)"

  n <- length(unique(df$StationCode))
  titley <- pr_relabel(unique(df$Parameters), style = "ggplot")

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleTime_Local, y = .data$Values)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::geom_point(ggplot2::aes(group = .data$StationCode, color = .data$StationCode)) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::labs(y = titley,
                  x = titlex) +
    ggplot2::scale_colour_manual(values = plotCols) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  return(p1)
}


#' Plot temporal Trends in plankton data
#'
#' @param df A dataframe containing the plankton timeseries data.
#' @param Trend Over what timescale to fit the Trend - "Raw", "Month" or "Year"
#' @param Survey "NRS" or "CPR" data
#' @param method Any method accepted by `geom_smooth()`
#' @param trans transformation of y axis on plot, whatever `scale_y_continuous()` trans accepts
#' #'
#' @return a timeseries plot
#'
#' @importFrom rlang "!!"
#'
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "Z") %>%
#'   dplyr::filter(Parameters == 'Biomass_mgm3')
#' pr_plot_Trends(df, Trend = "Month", Survey = "NRS")
#' pr_plot_Trends(df, Trend = "Year", Survey = "NRS")
#' pr_plot_Trends(df, Trend = "Raw", Survey = "NRS")
pr_plot_Trends <- function(df, Trend = "Raw", Survey = "NRS", method = "lm",  trans = "identity"){

  if (Trend == "Month"){
    Trend = "Month_Local"
  }
  if (Trend == "Year"){
    Trend = "Year_Local"
  }

  if (Survey == "CPR"){
    site = rlang::sym("BioRegion")
  } else if (Survey == "NRS"){
    site = rlang::sym("StationName")
  }

  titley <- pr_relabel(unique(df$Parameters))

  # Averaging based on `Trend` ----------------------------------------------

  if (Trend %in% c("Year_Local", "Month_Local")){
    df <- df %>%
      dplyr::group_by(!!rlang::sym(Trend), !!site) %>%
      dplyr::summarise(value = mean(.data$Values, na.rm = TRUE),
                       N = dplyr::n(),
                       sd = sd(.data$Values, na.rm = TRUE),
                       se = sd / sqrt(.data$N),
                       .groups = "drop")

  } else {
    Trend <- "SampleTime_Local" # Rename Trend to match the column with time
    df <- df %>%
      dplyr::group_by(.data$SampleTime_Local, !!site, .data$Parameters) %>% # accounting for microbial data different depths
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = "drop")%>%
      dplyr::rename(value = "Values")
  }

  # Do the plotting ---------------------------------------------------------

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(Trend), y = .data$value)) + # do this logging as in pr_plot_tsclimate
    ggplot2::geom_smooth(method = method, formula = y ~ x) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(rlang::enexpr(site), scales = "free_y", ncol = 1) +
    ggplot2::ylab(rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(Trend) %in% c("Month_Local")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(Trend) %in% "Year_Local"){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2) +
      ggplot2::xlab("Year")
  } else if (!rlang::as_string(Trend) %in% c("Month_Local", "Year_Local")){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::xlab("Year")
  }

  return(p1)
}


#' Plot single climatology
#'
#' @param df dataframe with specified time period, station code and parameter
#' @param Trend specified time period
#' @param Survey CPR or NRS data
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a climatology plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices(Survey = "NRS", Type = "P") %>%
#'         dplyr::filter(Parameters == "PhytoBiomassCarbon_pgL")
#' monthly <- pr_plot_Climatology(df, "NRS", "Month")
#'
#' df <- pr_get_Indices(Survey = "CPR", Type = "Z") %>%
#'         dplyr::filter(Parameters == "ZoopAbundance_m3")
#' annual <- pr_plot_Climatology(df, "CPR", "Year")
pr_plot_Climatology <- function(df, Survey = "NRS", Trend = "Month", trans = "identity"){

  if (Trend == "Month"){
    Trend = "Month_Local"
  }
  if (Trend == "Year"){
    Trend = "Year_Local"
  }

  # Trend <- dplyr::enquo(arg = Trend)
  Trend <- rlang::sym(Trend)

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::rename(StationCode = "BioRegion")
    plotCols <- colCPR
  } else if (Survey == "NRS"){
    plotCols <- colNRS
  }

  n <- length(unique(df$StationCode))
  title <- pr_relabel(unique(df$Parameters), style = "ggplot")

  df_climate <- df %>%
    dplyr::group_by(!!Trend, .data$StationCode) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")

  p1 <- ggplot2::ggplot(df_climate, ggplot2::aes(x = !!Trend, y = .data$mean, fill = .data$StationCode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se),
                           width = .2,                    # Width of the error bars
                           position = ggplot2::position_dodge(.9)) +
    ggplot2::labs(y = title) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::scale_fill_manual(values = plotCols)  +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  if("Month_Local" %in% colnames(df_climate)){
    p1 <- p1 +
      ggplot2::xlab("Month") +
      ggplot2::scale_x_continuous(breaks = seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  }

  if("Year_Local" %in% colnames(df_climate)){
    p1 <- p1 +
      ggplot2::xlab("Year") +
      ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))
  }

  return(p1)
}

#' Combined timeseries and climatology plots
#'
#' @param df data frame with SampleDate_Local, time period and parameter
#' @param Survey CPR or NRS data
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a combined plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices(Survey = "CPR", Type = "P") %>%
#'   dplyr::filter(Parameters == "PhytoAbundance_Cellsm3")
#' pr_plot_tsclimate(df, "CPR")
pr_plot_tsclimate <- function(df, Survey = "NRS", trans = "identity"){

  p1 <- pr_plot_TimeSeries(df, Survey, trans) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  p2 <- pr_plot_Climatology(df, Survey, "Month", trans) +
    ggplot2::theme(legend.position = "none")

  p3 <- pr_plot_Climatology(df, Survey, "Year", trans) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())

  plots <- patchwork::wrap_plots(p1, p2,  p3, nrow = 3)

  return(plots)
}

#' Time series plot of functional groups
#'
#' @param df dataframe in format of output from pr_get_FuncGroups
#' @param Scale y axis scale Actual or Percent
#' @param Trend Over what timescale to fit the Trend - "Raw", "Month" or "Year"
#'
#'
#' @return plot of fg timseries
#' @export
#'
#' @examples
#' df <- pr_get_FuncGroups("NRS", "P") %>% dplyr::filter(StationCode == 'PHB')
#' plot <- pr_plot_tsfg(df, "Actual")
#' plot
pr_plot_tsfg <- function(df, Scale = "Actual", Trend = "Raw"){

  if (Trend == "Month"){
    Trend = "Month_Local"
  }
  if (Trend == "Year"){
    Trend = "Year_Local"
  }

  n <- length(unique(df$Parameters))

  if("BioRegion" %in% colnames(df)){ # If CPR data
    SampleDate = rlang::sym("SampleTime_Local")
    station = rlang::sym("BioRegion")
    titley <- pr_relabel("PhytoAbundance_Cellsm3", style = "ggplot")

  } else { # If NRS data
    SampleDate = rlang::sym("SampleTime_Local")
    station = rlang::sym("StationName")
    titley <- pr_relabel("PhytoAbundance_CellsL", style = "ggplot")
  }

  titlex <- "Sample Time (Local)"
  if (Trend %in% c("Year_Local", "Month_Local")){

    df <- df %>%
      dplyr::group_by(!!rlang::sym(Trend), !!station, .data$Parameters) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       N = dplyr::n(),
                       sd = sd(.data$Values, na.rm = TRUE),
                       se = sd / sqrt(.data$N),
                       .groups = "drop")

  } else {
    Trend <- SampleDate # Rename Trend to match the column with time
  }

  if(Scale == "Percent") {
    df <- df %>%
      dplyr::group_by(!!rlang::sym(Trend), !!station, .data$Parameters) %>%
      dplyr::summarise(n = sum(.data$Values, na.rm = TRUE)) %>%
      dplyr::mutate(Values = .data$n / sum(.data$n, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else {
    df <- df %>%
      dplyr::mutate(Values = log10(.data$Values))
  }

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(Trend), y = .data$Values, fill = .data$Parameters)) +
    ggplot2::geom_area(alpha = 0.9 , linewidth = 0.2, colour = "white") +
    ggplot2::facet_wrap(rlang::enexpr(station), scales = "free", ncol = 1) +
    ggplot2::labs(y = titley) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(Trend) %in% c("Month_Local")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), expand = c(0, 0),
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(Trend) %in% c("Year_Local")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2, expand = c(0, 0)) +
      ggplot2::xlab("Year")
  } else if (rlang::as_string(Trend) %in% c("SampleTime_Local")){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
      ggplot2::xlab("Sample Date")
  }

  return(p1)
}

#' Essential Ocean Variables plot
#'
#' @param df dataframe containing timeseries data with Parameters and Values, output of pr_get_PolicyData and pr_get_Coeffs
#' @param EOV Essential OCean Variable as a parameter
#' @param Survey NRS, CPR or LTM 'Long term monitoring'
#' @param trans scale for y axis
#' @param col colour selection
#' @param labels do you want to print labels on the x axes
#'
#' @return plot of timeseries, anomalies and climatology
#' @export
#'
#' @examples
#' df <- pr_get_PolicyData("CPR") %>%
#'   pr_get_Coeffs()
#' pr_plot_EOV(df, EOV = "BiomassIndex_mgm3", Survey = "CPR",
#'       trans = "identity", col = "blue", labels = "no")
pr_plot_EOV <- function(df, EOV = "Biomass_mgm3", Survey = "NRS", trans = "identity", col = "blue", labels = "yes") {

  titley <- pr_relabel(EOV, style = "ggplot")

  if(Survey == "LTM"){
    lims <- c(lubridate::floor_date(min(df$SampleDate), "year"), lubridate::ceiling_date(Sys.Date(), "year"))
    df <- df %>%
      dplyr::filter(.data$Parameters == EOV)
  } else {
    lims <- c(lubridate::floor_date(min(df$SampleDate), "year"), lubridate::ceiling_date(Sys.Date(), "year"))
    df <- df %>%
      dplyr::filter(.data$Parameters == EOV)
  }

  p1 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$SampleDate, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$SampleDate, y = .data$fv), method = "lm", formula = "y ~ x", colour = col, fill = col, alpha = 0.5) +
    ggplot2::labs(x = "Year", y = rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::theme(legend.position = "none")

  if(labels == "no"){
    p1 <- p1 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if(Survey == "LTM"){
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims)
  } else {
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims)
  }

  p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleDate, .data$anomaly)) +
    ggplot2::geom_col(fill = col, colour = col, alpha = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::labs(y = "Anomaly")

  if(labels == "no"){
    p2 <- p2 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if(Survey == "LTM"){
    p2 <-  p2 + ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims)
  } else {
    p2 <-  p2 + ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims)
  }

  p3 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$Month, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$Month, y = .data$fv), method = "loess", formula = "y ~ x", colour = col, fill = col, alpha = 0.5) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::scale_x_continuous(breaks = seq(0.5, 6.3, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::xlab("Month") +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  if(labels == "no"){
    p3 <- p3 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  plots <- patchwork::wrap_plots(p1, p2, p3, widths = c(3,3,2))

  return(plots)


}



#' Combined timeseries and climatology plots for environmental variables
#'
#' @param df A dataframe from pr_get_nuts or pr_get_pigments
#' @param Trend Trend line to be used, options None, Smoother, Linear
#' @param trans scale on which to plot y axis
#'
#' @return A plot with timeseries and climatology at depths
#' @export
#'
#' @examples
#' df <- pr_get_NRSChemistry() %>% dplyr::filter(Parameters == "SecchiDepth_m")
#' pr_plot_Enviro(df)
#'
pr_plot_Enviro <- function(df, Trend = "None", trans = "identity") {

  n <- length(unique(df$StationName))

  titley <- pr_relabel(unique(df$Parameters), style = 'ggplot')

  np <- length(unique(df$SampleDepth_m))

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleTime_Local, .data$Values, colour = .data$StationName)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Year", y = titley) +
    ggplot2::facet_grid(SampleDepth_m ~., scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::scale_colour_manual(values = colNRS)

  if(Trend == "Smoother"){
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x)
  }
  if(Trend == "Linear"){
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
  }


  mdat <- df %>%
    dplyr::group_by(.data$StationName, .data$Month_Local, .data$SampleDepth_m, .data$Parameters) %>%
    dplyr::summarise(MonValues = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")

  m <- ggplot2::ggplot(mdat, ggplot2::aes(.data$Month_Local, .data$MonValues, colour = .data$StationName)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(.data$SampleDepth_m ~., scales = "free") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x) +
    ggplot2::scale_x_continuous(breaks = seq(1,12,length.out = 12),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::scale_colour_manual(values = colNRS) +
    ggplot2::labs(x = "Month") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   legend.position = 'none',
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(face = "bold", size = 12, angle = 0))

  plots <- p + m + patchwork::plot_layout(widths = c(3,1), guides = 'collect', heights = np * 200)

  return(plots)

}


#' Contour plots for depth stratified environmental data
#'
#' @param df dataframe from pr_get_NRSEnvContour
#' @param Interpolation If TRUE data is interpolated at 5m intervals (DAR = 10m)
#'
#' @return a contour plot
#' @export
#'
#' @examples
#' df <- pr_get_NRSEnvContour("Pico") %>% dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
#' StationCode %in% c('YON', 'PHB', 'NSI'))
#' plot <- pr_plot_NRSEnvContour(df, Interpolation = TRUE)
pr_plot_NRSEnvContour <- function(df, Interpolation = TRUE) {
  stations <- unique(as.character(df$StationName))
  param <- planktonr::pr_relabel(unique(df$Parameters), style = 'ggplot')

  if (Interpolation == FALSE) {
    df <- df %>%
      dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values),
                    SampleDepth_m = ifelse(.data$StationName %in% c('Darwin'), round(.data$SampleDepth_m/10, 0)*10,
                                           round(.data$SampleDepth_m/5, 0)*5))
    minDate <- lubridate::floor_date(min(df$SampleTime_Local, na.rm = TRUE), unit = 'month')
    df <- df %>% dplyr::mutate(MonthSince = lubridate::interval(minDate, .data$SampleTime_Local) %/% months(1),
                               Year = lubridate::year(.data$SampleTime_Local),
                               Label = ifelse(.data$Month_Local == 6, .data$Year, NA))
    myBreaks <- (df %>% dplyr::filter(!is.na(.data$Label)) %>% dplyr::distinct(.data$MonthSince) %>%
                   dplyr::arrange(dplyr::desc(-.data$MonthSince), .by_group = FALSE))$MonthSince
    Label <- (df %>% dplyr::group_by(.data$Label) %>% dplyr::summarise(n = dplyr::n()) %>% tidyr::drop_na() %>%
                dplyr::distinct(.data$Label))$Label

  } else {
    plotfunc <- function(stations) {
      df <- df %>% dplyr::filter(.data$StationName == stations) %>%
        dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values),
                      SampleDepth_m = ifelse(.data$StationName %in% c('Darwin'), round(.data$SampleDepth_m/10, 0)*10,
                                             round(.data$SampleDepth_m/5, 0)*5))

      maxDepth <- max(df$SampleDepth_m, na.rm = TRUE)
      Depths <- unique(df$SampleDepth_m)
      minDate <-
        lubridate::floor_date(min(df$SampleTime_Local, na.rm = TRUE), unit = 'month')
      maxDate <-
        lubridate::floor_date(max(df$SampleTime_Local, na.rm = TRUE), unit = 'month')
      seqDate <-
        seq.Date(as.Date(minDate), as.Date(maxDate), by = 'month')

      seqDepth <- Depths

      depthPurr <- function(seqDepth) {
        interped <-
          expand.grid(SampleTime_Local = seqDate,
                      SampleDepth_m = Depths) %>%
          data.frame() %>%
          dplyr::left_join(df,
                    by = c("SampleDepth_m", "SampleTime_Local"))  %>%
          dplyr::filter(.data$SampleDepth_m == seqDepth) %>%
          dplyr::arrange(.data$SampleTime_Local) %>%
          dplyr::mutate(
            Values = zoo::na.approx(.data$Values, maxgap = 5, na.rm = FALSE),
            StationName = ifelse(
              is.na(.data$StationName),
              stations,
              as.character(.data$StationName)
            )
          )
      }

      PlotData <- purrr::map_dfr(seqDepth, depthPurr) # Interpolating across time for each depth for the station
    }

    PlotData <- purrr::map_dfr(stations, plotfunc) # Running the interpolation for each station

    minDateAll <- lubridate::floor_date(min(PlotData$SampleTime_Local, na.rm = TRUE), unit = 'month')

    df <- PlotData %>% dplyr::mutate(MonthSince = lubridate::interval(minDateAll, .data$SampleTime_Local) %/% months(1),
                                           Year = lubridate::year(.data$SampleTime_Local),
                                           Label = ifelse(.data$Month_Local == 6, .data$Year, NA)) %>%
      droplevels() %>%
      planktonr::pr_reorder()

    Label <- (df %>% dplyr::group_by(.data$Label) %>% dplyr::summarise(n = dplyr::n()) %>% tidyr::drop_na() %>%
                dplyr::distinct(.data$Label))$Label
    myBreaks <- (df %>% dplyr::filter(!is.na(.data$Label)) %>% dplyr::distinct(.data$MonthSince) %>%
                   dplyr::arrange(dplyr::desc(-.data$MonthSince), .by_group = FALSE))$MonthSince
  }

  # Function for plots
  outPlot <- function(df, x, just = "left"){
      out <- ggplot2::ggplot(data = df, ggplot2::aes(x, y = .data$SampleDepth_m, z = .data$Values)) +
        ggplot2::geom_contour_filled(bins = 8) +
        ggplot2::geom_point(colour = 'dark grey', cex = 0.75) +
        ggplot2::facet_grid(.data$StationName ~ ., scales = 'free') +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8),
                       legend.justification = just) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = param, title.position = 'top')) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
    out
  }

  # Plotting year time series
  out <- outPlot(df, df$MonthSince, "left") +
    ggplot2::labs(x = "Year", y = 'Depth (m)') +
    ggplot2::theme(strip.text = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(breaks = myBreaks, labels = Label, expand = c(0, 0))

  # Prepare monthly climatology data
  dfMon <- df %>%
    dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values),
                  SampleDepth_m = ifelse(.data$StationName == 'Darwin', round(.data$SampleDepth_m/10, 0)*10,
                                         round(.data$SampleDepth_m/5, 0)*5)) %>%
    dplyr::group_by(.data$Month_Local, .data$StationName, .data$SampleDepth_m) %>%
    dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                     .groups = 'drop')

  # Plotting monthly climatology
  outMon <- outPlot(dfMon, dfMon$Month_Local, "right") +
    ggplot2::labs(x = "Month") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
  outMon

  np <- length(stations)
  plots <- out + outMon + patchwork::plot_layout(widths = c(3,1), heights = np * 200)

  return(plots)

}



#' Frequency plot of the selected species
#'
#' @param df dataframe of format similar to output of pr_get_fmap_data()
#' @param species species to plot
#' @param interactive ggplot if false, plotlist of leaflets if true
#'
#' @return a plot of frequency of occurrence of chosen species
#' @export
#'
#' @examples
#' df <- data.frame(Long = c(110, 130, 155, 150), Lat = c(-10, -35, -27, -45),
#'                  freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
#'                  Season = c("December - February","March - May",
#'                  "June - August","September - November"),
#'                  Taxon = 'Acartia danae',
#'                  Survey = 'CPR')
#' plot <- pr_plot_FreqMap(df, species = 'Acartia danae', interactive = FALSE)
pr_plot_FreqMap <- function(df, species, interactive = TRUE){

  df <- df %>%
    dplyr::mutate(Taxon = dplyr::if_else(.data$Taxon == 'Taxon', species, .data$Taxon)) %>%
    dplyr::filter(.data$Taxon %in% species)  %>%
    dplyr::arrange(dplyr::desc(.data$freqfac)) %>%
    dplyr::group_by(.data$Season, .data$Survey, .data$Lat, .data$Long, .data$Taxon) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$freqfac)

  if(interactive == FALSE){
    cols <- c("lightblue1" ,"skyblue3", "dodgerblue2","blue1", "navyblue")

    Species <- unique(df$Taxon)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = MapOz) +
      ggplot2::geom_point(data=df, ggplot2::aes(x=.data$Long, y=.data$Lat, colour=.data$freqfac, pch = .data$Survey), size = 2) +
      ggplot2::facet_wrap( ~ .data$Season, dir = "v") +
      ggplot2::labs(title = Species) +
      ggplot2::scale_colour_manual(name = "", values = cols, drop = FALSE) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     title = ggplot2::element_text(face = "italic"),
                     legend.title = ggplot2::element_text(face = "plain", size = 12),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "snow1"),
                     legend.position = "bottom",
                     legend.key = ggplot2::element_blank())
    return(p)

  } else {

    df <- df %>% dplyr::group_split(.data$Season)

    plotlist <- function(dflist){

      CPRpal <- leaflet::colorFactor(c("lightblue1", "skyblue3", "dodgerblue2", "blue1", "navyblue"), domain = dflist$freqfac)
      NRSpal <- leaflet::colorFactor(c("#CCFFCC", "#99FF99", "#669933", "#009900", "#006600"), domain = dflist$freqfac)

      dfCPR <- dflist %>% dplyr::filter(.data$Survey == 'CPR')
      dfNRS <- dflist %>% dplyr::filter(.data$Survey == 'NRS')

      title1 <- htmltools::div(
        htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title1 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                font-weight: bold;
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
        unique(dflist$Season))

      title2 <- htmltools::div(
        htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title2 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                # font-weight: bold;
                                                font-style: italic;
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
        unique(dflist$Taxon))

      fmap <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
        leaflet::addPolygons(data = mbr,  group = "Marine Bioregions",
                             color = ~Colour, fill = ~Colour,
                             opacity = 1, fillOpacity = 0.3,
                             weight = 1) %>%
        leaflet::addCircleMarkers(data = dfCPR, group = 'Continuous Plankton Recorder',
                                  lat = ~ Lat, lng = ~ Long,
                                  radius = ~ifelse(freqfac == "Absent", 2, 5),
                                  color = ~CPRpal(freqfac),
                                  fill = ~CPRpal(freqfac)) %>%
        leaflet::addCircleMarkers(data = dfNRS , group = 'National Reference Stations',
                                  lat = ~ Lat, lng = ~ Long,
                                  color = ~NRSpal(freqfac),
                                  radius = ~ifelse(freqfac == "Absent", 1, 5)) %>%
        leaflet::addControl(title1,
                            position = "topright",
                            className = "map-title1") %>%
        leaflet::addControl(title2,
                            position = "topright",
                            className = "map-title2")  %>%
        leaflet::addLayersControl( # Layers control
          overlayGroups = c("National Reference Stations", "Continuous Plankton Recorder", "Marine Bioregions"),
          position = "bottomleft",
          options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA))
    }

    plotlist <- purrr::map(df, plotlist)

    return(plotlist)
  }
}


#' Plot of relative day and night abundances
#'
#' @param df dataframe as output of pr_get_DayNight() filtered for one species
#'
#' @return plot of relative day and night abundances
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(seq(1,12,1),2), daynight = c(rep('day', 12), rep('night', 12)),
#' CopeAbundance_m3 = runif(24, 0.1, 10), Species = 'Acartia danae')
#' plot <- pr_plot_DayNight(df)
pr_plot_DayNight <-  function(df){

  titlemain <- unique(df$Species)
  if("CopeAbundance_m3" %in% names(df)){
    ylabel <- pr_relabel("CopeAbundance_m3", style = "ggplot") # this is probably only worth doing for copepods as we don"t have a lot of data for other things
  } else {
    ylabel <- pr_relabel("PhytoAbund_m3", style = "ggplot")
  }

  plots <- ggplot2::ggplot(df, ggplot2::aes(.data$Month_Local, .data$Species_m3)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(formula = "y ~ x", method = "loess") +
    ggplot2::facet_grid(~ .data$daynight, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks= seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = ylabel, x = "Month") +
    ggplot2::ggtitle(titlemain) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))

}


#' Plot of STI kernel density for species
#'
#' @param df dataframe as output of pr_get_STIdata() filtered for one species
#'
#' @return plot of STI kernel density
#' @export
#'
#' @examples
#' df <- data.frame(SST = runif(24, 5, 25),
#'                  Project = c(rep("CPR", 12), rep("NRS", 12)),
#'                  Species_m3 = runif(24, 0.1, 10),
#'                  Species = 'Acartia danae')
#'
#' plot <- pr_plot_STI(df)
pr_plot_STI <-  function(df){
  means <- df %>%
    dplyr::group_by(.data$Project) %>%
    dplyr::summarise(mean = mean(.data$Species_m3, na.rm = TRUE))

  #means are so different so log data as the abundance scale is so wide

  sti <- df %>%
    dplyr::left_join(means, by = "Project") %>%
    dplyr::mutate(relab = .data$Species_m3/.data$mean) %>%
    dplyr::group_by(.data$SST, .data$Species) %>%
    dplyr::summarize(relab = sum(.data$relab),
                     freq = dplyr::n(),
                     a = sum(.data$relab)/dplyr::n(),
                     .groups = "drop")

  n <- length(sti$SST)
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
                  density(SST, weight=weight,
                          bw=kernBw,
                          from=kernMin,
                          to=kernMax,
                          n=kernN))

  z <- data.frame(kernTemps, y = kernOut$y)
  STI <- round(z[which.max(z[,2]),]$kernTemps,1)

  # looks at these lines if we want to plot on an abundance scale instead of 0 - 1 scale
  #  kern_yp[,i] <- kernOut$y/sum(kernOut$y) * 100 * mean(sti$relab)
  #  kypout[,i] <- kernOut$y

  xlabel <- pr_relabel("Temperature_degC", style = "ggplot")
  subtit <- rlang::expr(paste("STI = ",!!STI, "\U00B0","C"))

  stiplot <- ggplot2::ggplot(z, ggplot2::aes(kernTemps, .data$y)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = STI, colour = "blue", lty = 4) +
    ggplot2::theme_bw() +
    ggplot2::labs(x=xlabel, y="Relative kernel density") +
    ggplot2::ggtitle(taxon, subtitle = subtit) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))

  return(stiplot)

}


#' IMOS progress plot
#'
#' @param df output from pr_get_ProgressMapData
#' @param interactive Should the plot be interactive with leaflet?
#'
#' @return a plot of IMOS progress
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMapData("CPR")
#' plot <- pr_plot_ProgressMap(df)
pr_plot_ProgressMap <- function(df, interactive = FALSE){

  if (interactive == TRUE){

    df_CPR <- df %>%
      dplyr::filter(.data$Survey == "CPR" & (!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL)))

    df_PCI <- df %>%
      dplyr::filter(.data$Survey == "CPR" & is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))


    df_NRS <- df %>% dplyr::filter(.data$Survey == "NRS")
    rm(df)

    labs_cpr <- lapply(seq(nrow(df_CPR)), function(i) {
      paste("<strong>Sample Date:</strong>", df_CPR$SampleTime_Local[i], "<br>",
            "<strong>Bioregion:</strong>", df_CPR$Name[i], "<br>",
            "<strong>Latitude:</strong>", df_CPR$Latitude[i], "<br>",
            "<strong>Longitude:</strong>", df_CPR$Longitude[i], "<br>",
            "<strong>Phytoplankton Abundance (L\u207B\u00B9)</strong>: ", round(df_CPR$PhytoAbundance_CellsL[i],2), "<br>",
            "<strong>Zooplankton Abundance (m\u207B\u00B3)</strong>: ", round(df_CPR$ZoopAbundance_m3[i],2), "<br>",
            "<strong>Phytoplankton Colour Index:</strong>", df_CPR$PCI[i], "<br>")})

    labs_cpr2 <- lapply(seq(nrow(df_CPR)), function(i) {
      paste("<strong>Sample Date:</strong>", df_CPR$SampleTime_Local[i], "<br>",
            "<strong>Bioregion:</strong>", df_CPR$Name[i], "<br>",
            "<strong>Latitude:</strong>", df_CPR$Latitude[i], "<br>",
            "<strong>Longitude:</strong>", df_CPR$Longitude[i], "<br>",
            "<strong>Phytoplankton Colour Index:</strong>", df_CPR$PCI[i], "<br>")})

    labs_mbr <- lapply(seq(nrow(mbr)), function(i) {
      if (mbr$REGION[i] != "Southern Ocean Region"){
        br = " Bioregion"
      } else {
        br = ""
      }
      sapply(strwrap(
        paste("<strong>The ", mbr$REGION[i], br,"</strong>",
              "is characterised by",(CPRinfo %>% dplyr::filter(mbr$REGION[i] == .data$BioRegion))$Features, "<br>"),
        width = 60, simplify = FALSE),
        paste, collapse = "<br>")
    })

    title1 <- htmltools::div(
      htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title1 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                font-weight: bold;
                                                font-size: 24px;
                                                margin: 0;
                                                margin-right: 6px}")),
      "Plankton sampling progress")

    title2 <- htmltools::div(
      htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title2 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                # font-weight: bold;
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
      "Hover cursor over items of interest")

    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
      leaflet::addPolygons(data = mbr,  group = "Marine Bioregions",
                           color = ~Colour,
                           fill = ~Colour,
                           opacity = 0.4,
                           fillOpacity = 0.4,
                           weight = 1,
                           label = lapply(labs_mbr, htmltools::HTML)) %>%
      leaflet::addCircleMarkers(data = df_CPR,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 3, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (Phyto/Zoo Counts)",
                                label = lapply(labs_cpr, htmltools::HTML)) %>%
      leaflet::addCircleMarkers(data = df_PCI,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 1, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (PCI Only)",
                                label = lapply(labs_cpr2, htmltools::HTML)) %>%
      leaflet::addAwesomeMarkers(data = df_NRS,
                                 lat = ~ Latitude,
                                 lng = ~ Longitude,
                                 group = "National Reference Stations",
                                 clusterOptions = leaflet::markerClusterOptions(showCoverageOnHover = FALSE,
                                                                                spiderfyOnMaxZoom = FALSE,
                                                                                maxClusterRadius = 40)) %>%
      leaflet::addControl(title1,
                          position = "topright",
                          className = "map-title1"
      ) %>%
      leaflet::addControl(title2,
                          position = "topright",
                          className = "map-title2"
      ) %>%
      leaflet::addLayersControl( # Layers control
        overlayGroups = c("National Reference Stations",
                          "Continuous Plankton Recorder (Phyto/Zoo Counts)",
                          "Continuous Plankton Recorder (PCI Only)",
                          "Marine Bioregions"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA)) %>%
      leaflet::addMiniMap() %>%  # add a minimap
      leaflegend::addLegendFactor(pal = leaflet::colorFactor("#FFA500", "National Reference Stations"),
                                  shape = "circle", values = "National Reference Stations") %>%
      leaflet::addLegend("topleft",
                         colors = mbr$Colour,
                         labels = mbr$REGION,
                         title = "Bioregions",
                         opacity = 1) %>%
      leaflet::hideGroup("Continuous Plankton Recorder (PCI Only)")



    return(map)

  } else {

    MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                         returnclass = "sf")

    PMapData2 <- df %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

    PMapSum <- merge(df %>% dplyr::group_by(.data$Region, .data$Survey) %>%
                       dplyr::summarise(Sums = dplyr::n(),
                                        .groups = "drop") %>%
                       dplyr::mutate(label = paste0(.data$Region, ' = ', .data$Sums)),
                     df %>% dplyr::group_by(.data$Region, .data$Survey) %>%
                       dplyr::summarise(Lats = mean(.data$Latitude),
                                        Lons = mean(.data$Longitude),
                                        .groups = "drop")) %>%
      sf::st_as_sf(coords = c("Lons", "Lats"), crs = 4326)

    nudgex = c(-2,5.5,2,8.5,9.5,0,0,4)
    nudgey = c(-3,0,1,0,0,7,-2,10)
    # GAB, GBR, NA, NEAC, SEAC, SO, Tas, WA

    Survey <- df %>%
      dplyr::select("Survey") %>%
      dplyr::distinct()

    gg <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
      ggplot2::coord_sf(xlim = c(105, 170), ylim = c(-54, -7), expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank())

    if ("NRS" %in% Survey$Survey) {
      gg <-  gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "NRS"), size = 5) +
        ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "NRS"),
                              size = 5, ggplot2::aes(label = .data$label),
                              show.legend = FALSE, nudge_x = 3)
    }

    if ("CPR" %in% Survey$Survey) {

      gg <-  gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "CPR"),
                         size = 1, ggplot2::aes(color =.data$Region), show.legend = FALSE) +
        ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "CPR"),
                              size = 5, ggplot2::aes(label = .data$label, color = .data$Region),
                              show.legend = FALSE, check_overlap = TRUE, nudge_x = nudgex, nudge_y = nudgey)
    }

    return(gg)
  }

}



#' Plot Gantt Chart showing plankton sampling status
#'
#' @param dat Trip data for either NRS or CPR.
#' @param Survey "NRS" or "CPR"
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dat <- pr_get_CPRTrips()
#' gg <- pr_plot_Gantt(dat, Survey = "CPR")
#' dat <- pr_get_NRSTrips()
#' gg <- pr_plot_Gantt(dat, Survey = "NRS")
pr_plot_Gantt <- function(dat, Survey = "NRS"){

  if (Survey == "CPR"){
    dat2 <- dat %>%
      dplyr::arrange(.data$Latitude) %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local/12) %>%
      dplyr::distinct(.data$YearMonth, .data$Region, .data$TripCode) %>%
      dplyr::group_by(.data$YearMonth, .data$Region, .data$TripCode) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup()

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$Region, width = 1/12, height = 2/12), fill = "black", colour = "black") +
      ggplot2::geom_tile() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("Continuous Plankton Counter Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return (gg)


  } else if (Survey == "NRS"){

    dat2 <- pr_get_NRSTrips(Type = c("P", "Z")) %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local/12) %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      dplyr::group_by(.data$YearMonth, .data$StationName) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$StationName, width = 1/12, height = 2/12), fill = "black", colour = "black") +
      ggplot2::geom_tile() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("National Reference Station Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return(gg)

  }

}



# 'Taxa Accumulation Curve
#'
#' Plot a taxa accumulation curve for everything that is identified by the IMOS plankton team
#'
#' @param dat A dataframe of plankton data
#' @param Survey "CPR" or "NRS"
#' @param Type "P" or "Z"
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' dat <- pr_get_TaxaAccum(Survey = "NRS", Type = "Z")
#' p <- pr_plot_TaxaAccum(dat, Survey = "NRS", Type = "Z")
#' dat <- pr_get_TaxaAccum(Survey = "CPR", Type = "P")
#' p <- pr_plot_TaxaAccum(dat, Survey = "CPR", Type = "P")
pr_plot_TaxaAccum <- function(dat, Survey = "NRS", Type = "Z"){

  dat <- dat %>%
    tidyr::pivot_longer(-pr_get_NonTaxaColumns(Survey = Survey, Type = Type), names_to = "Taxa", values_to = "Abundance") %>%
    dplyr::filter(.data$Abundance > 0) %>%
    dplyr::arrange(.data$SampleTime_Local) %>%
    dplyr::group_by(.data$Taxa) %>%
    dplyr::summarise(First = dplyr::first(.data$SampleTime_Local), .groups = "drop") %>%
    dplyr::arrange(.data$First) %>%
    dplyr::mutate(RowN = dplyr::row_number())


  gg <- ggplot2::ggplot(data = dat, ggplot2::aes(x = .data$First, y = .data$RowN)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = "Year", breaks = "2 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::ylab("Taxa Identified") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle(paste(Survey, "-", planktonr::pr_title(Type)))

  return(gg)

}






