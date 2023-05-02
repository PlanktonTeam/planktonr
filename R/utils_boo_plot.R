
#' Sidebar panel plot of selected NRS stations
#'
#' @param df dataframe containing station codes to plot
#' @param Survey NRS or Coastal
#'
#' @return a map of the selected stations
#' @export
#'
#' @examples
#' df <- data.frame(StationCode = c("MAI", "PHB"))
#' pmap <- pr_plot_NRSmap(df)
pr_plot_NRSmap <- function(df, Survey = 'NRS'){

  if(Survey == 'NRS'){
    meta_sf <- meta_sf
  } else {
    meta_sf <- csDAT
  }

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



#' PCI plot for CPR data
#'
#' @param df dataframe with location and seasonal PCI data
#'
#' @return plot of PCI around Australia
#' @export
#'
#' @examples
#' pr_get_PCIData() %>% pr_plot_PCI()
pr_plot_PCI <- function(df){
  cprmap <-  ggplot2::ggplot() +
    ggplot2::geom_raster(data = df, ggplot2::aes(x = .data$Longitude, y = .data$Latitude, fill = .data$PCI), interpolate = TRUE) +
    ggplot2::scale_fill_gradient(low = "light green", high = "darkgreen",
                                 breaks = c(0,1,2,3),
                                 labels = c("No Colour", "Very Pale Green", "Pale Green", "Green")) +
    ggplot2::facet_wrap(~ .data$Season) +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    theme_pr() +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 20)) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  return(cprmap)
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
#'   dplyr::filter(Parameters == "Biomass_mgm3", StationCode %in% c("NSI", "PHB"))
#' timeseries <- pr_plot_TimeSeries(df, Survey = "NRS")

pr_plot_TimeSeries <- function(df, Survey = "NRS", trans = "identity"){

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::rename(StationName = "BioRegion")
    plotCols <- colCPR
    ltype <- "solid"

  } else {
    df <- df %>%
      dplyr::group_by(.data$SampleTime_Local, .data$StationName, .data$Parameters) %>% # accounting for microbial data different depths
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = "drop")
    plotCols <- colNRSName
    ltype <- ltyNRSName
  }

  titlex <- "Sample Date (Local)"

  n <- length(unique(df$StationName))
  titley <- pr_relabel(unique(df$Parameters), style = "ggplot")

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleTime_Local, y = .data$Values)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$StationName, color = .data$StationName, linetype = .data$StationName)) +
    ggplot2::geom_point(ggplot2::aes(group = .data$StationName, color = .data$StationName)) +
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    ggplot2::labs(y = titley,
                  x = titlex) +
    ggplot2::scale_colour_manual(values = plotCols, limits = force) +
    ggplot2::scale_shape_manual(values = ltype) +
    theme_pr()

  if(Survey != "Coastal") {
    p1 +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0))
  } else {
    p1 +
      ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0))
  }

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
  } else if (Survey != "CPR"){
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
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    theme_pr() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(Trend) %in% c("Month_Local")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), expand = c(0, 0),
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(Trend) %in% "Year_Local"){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2, expand = c(0, 0)) +
      ggplot2::xlab("Year")
  } else if (!rlang::as_string(Trend) %in% c("Month_Local", "Year_Local") & Survey != 'Coastal'){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
      ggplot2::xlab("Year")
  } else if (!rlang::as_string(Trend) %in% c("Month_Local", "Year_Local") & Survey == 'Coastal'){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
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
#' dplyr::filter(Parameters == "PhytoBiomassCarbon_pgL", StationCode %in% c("NSI", "PHB"))
#'
#' monthly <- pr_plot_Climatology(df, "NRS", "Month")
#'
#' df <- pr_get_Indices(Survey = "CPR", Type = "Z") %>%
#'         dplyr::filter(Parameters == "ZoopAbundance_m3")
#' annual <- pr_plot_Climatology(df, "CPR", "Year")
pr_plot_Climatology <- function(df, Survey = "NRS", Trend = "Month", trans = "identity"){

  if (Trend == "Month"){
    Trend = "Month_Local"
    dodge <- 0.8
  }
  if (Trend == "Year"){
    Trend = "Year_Local"
    dodge <- 300
  }

  # Trend <- dplyr::enquo(arg = Trend)
  Trend <- rlang::sym(Trend)

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::rename(StationName = "BioRegion")
    plotCols <- colCPR
  } else if (Survey != "CPR"){
    plotCols <- colNRSName
  }

  n <- length(unique(df$StationName))
  title <- pr_relabel(unique(df$Parameters), style = "ggplot")

  df_climate <- df %>%
    dplyr::group_by(!!Trend, .data$StationName) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop") %>%
    tidyr::complete(!!Trend, .data$StationName)

  if("Year_Local" %in% colnames(df_climate)){
    df_climate <- df_climate %>%
      dplyr::mutate(!!Trend := lubridate::as_date(paste(!!Trend, 1, 1, sep = "-"))) #TODO Temp fix to convert to date and fix ticks below
  }

  p1 <- ggplot2::ggplot(df_climate, ggplot2::aes(x = !!Trend, y = .data$mean, fill = .data$StationName,
                                                 group = .data$StationName)) +
    ggplot2::geom_col(width = dodge, position = ggplot2::position_dodge(width = dodge)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se),
                           width = dodge/3,                    # Width of the error bars
                           position = ggplot2::position_dodge(width = dodge)) +
    ggplot2::labs(y = title) +
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = plotCols, limits = force, guide = ggplot2::guide_legend(byrow = TRUE)) +
    theme_pr()

  if("Month_Local" %in% colnames(df_climate)){
    p1 <- p1 +
      ggplot2::xlab("Month") +
      ggplot2::scale_x_continuous(breaks = seq(1,12, length.out = 12), expand = c(0, 0),
                                  labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  }

  if("Year_Local" %in% colnames(df_climate) & Survey != 'Coastal'){
    p1 <- p1 +
      ggplot2::xlab("Year") +
      ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0))
  }
  if("Year_Local" %in% colnames(df_climate) & Survey == 'Coastal'){
    p1 <- p1 +
      ggplot2::xlab("Year") +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0))
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

  plots <- patchwork::wrap_plots(p1, p2,  p3, nrow = 3) & theme_pr()

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

    if ("Copepod" %in% df$Parameters) {
      titley <- pr_relabel("ZoopAbund_m3", style = "ggplot")
    } else {
      titley <- pr_relabel("PhytoAbundance_Cellsm3", style = "ggplot")
    }

  } else { # If NRS data
    SampleDate = rlang::sym("SampleTime_Local")
    station = rlang::sym("StationName")

    if ("Copepod" %in% df$Parameters) {
      titley <- pr_relabel("ZoopAbund_m3", style = "ggplot")
    } else {
      titley <- pr_relabel("PhytoAbundance_CellsL", style = "ggplot")
    }
  }

  # Overwrite titley if its a proportion
  if (Scale == "Percent"){
    titley <- "Proportion"
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
    theme_pr() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
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
#' @param df dataframe containing timeseries data with Parameters and Values, output of pr_get_EOVs and pr_get_Coeffs
#' @param EOV Essential Ocean Variable as a parameter
#' @param Survey NRS, CPR or LTM 'Long term monitoring'
#' @param trans scale for y axis
#' @param col colour selection
#' @param labels do you want to print labels on the x axes
#'
#' @return plot of timeseries, anomalies and climatology
#' @export
#'
#' @examples
#' df <- pr_get_EOVs("CPR") %>% pr_remove_outliers(2) %>%
#'   pr_get_Coeffs()
#' pr_plot_EOVs(df, EOV = "chl_oc3", Survey = "CPR",
#'       trans = "identity", col = "blue", labels = "no")
pr_plot_EOVs <- function(df, EOV = "Biomass_mgm3", Survey = "NRS", trans = "identity", col = "blue", labels = "yes") {

  titley <- pr_relabel(EOV, style = "ggplot")

  if(Survey == "LTM"){
    lims <- c(lubridate::floor_date(min(df$SampleDate), "year"), lubridate::ceiling_date(max(df$SampleDate), "year"))
    df <- df %>%
      dplyr::filter(.data$Parameters == EOV)
  } else {
    lims <- c(lubridate::floor_date(min(df$SampleDate), "year"), lubridate::ceiling_date(max(df$SampleDate), "year"))
    df <- df %>%
      dplyr::filter(.data$Parameters == EOV)
  }

  p1 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$SampleDate, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$SampleDate, y = .data$fv), method = "lm", formula = "y ~ x", colour = col, fill = col, alpha = 0.5) +
    ggplot2::labs(x = "Year", subtitle = rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    theme_pr() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank(),
                   #plot.subtitle = ggplot2::element_text(size = 12)
    )

  if(labels == "no"){
    p1 <- p1 +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if(Survey == "LTM"){
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims, expand = c(0, 0))
  } else {
    p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims, expand = c(0, 0))
  }

  p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleDate, .data$anomaly)) +
    ggplot2::geom_col(fill = col, colour = col, alpha = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::labs(y = "Anomaly") +
    theme_pr()

  if(labels == "no"){
    p2 <- p2 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if(Survey == "LTM"){
    p2 <- p2 +
      ggplot2::scale_x_datetime(date_breaks = "10 years", date_labels = "%Y", limits = lims, expand = c(0, 0))
  } else {
    p2 <- p2 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims, expand = c(0, 0))
  }

  p3 <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$Month, y = .data$Values), colour = col) +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$Month, y = .data$fv), method = "loess", formula = "y ~ x", colour = col, fill = col, alpha = 0.5) +
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = seq(0.5, 6.3, length.out = 12), expand = c(0, 0),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::xlab("Month") +
    theme_pr() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  if(labels == "no"){
    p3 <- p3 +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
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

  titley <- pr_relabel(unique(df$Parameters), style = "ggplot")

  np <- length(unique(df$SampleDepth_m))

  df <- df %>%
    dplyr::mutate(SampleDepth_ms = stringr::str_c(.data$SampleDepth_m," m"))

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleTime_Local, .data$Values, colour = .data$StationName)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Year", y = titley) +
    ggplot2::facet_wrap(.data$SampleDepth_ms ~., scales = "free_y", ncol = 1, strip.position = "right") +
    theme_pr() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(trans = trans, expand = c(0, 0)) +
    ggplot2::scale_colour_manual(values = colNRSName, limits = force)

  if(Trend == "Smoother"){
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x)
  }
  if(Trend == "Linear"){
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
  }

  mdat <- df %>%
    dplyr::group_by(.data$StationName, .data$Month_Local, .data$SampleDepth_ms, .data$Parameters) %>%
    dplyr::summarise(MonValues = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")

  m <- ggplot2::ggplot(mdat, ggplot2::aes(.data$Month_Local, .data$MonValues, colour = .data$StationName)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(.data$SampleDepth_ms ~., scales = "free_y", ncol = 1, strip.position = "right") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x) +
    ggplot2::scale_x_continuous(breaks = seq(1,12,length.out = 12), expand = c(0, 0),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::scale_colour_manual(values = colNRSName, limits = force) +
    ggplot2::labs(x = "Month") +
    theme_pr() +
    ggplot2::theme(legend.position = 'none',
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(face = "bold", angle = 0)) # size = 12

  plots <- p +
    m + patchwork::plot_layout(widths = c(3,1), heights = np * 200)

  return(plots)

}


#' Contour plots for depth stratified environmental data
#'
#' @param df dataframe from pr_get_NRSEnvContour
#' @param Interpolation If TRUE data is interpolated at 5m intervals (DAR = 10m)
#' @param Fill_NA to fill in NA's or not via zoo::na_approx, only used when interpolation is TRUE
#' @param maxGap maximum gap across which to interpolate across NAs
#'
#' @return a contour plot
#' @export
#'
#' @examples
#' df <- pr_get_NRSEnvContour("Chemistry") %>% dplyr::filter(Parameters == "Nitrate_umolL",
#' StationCode %in% c('YON', 'MAI', 'PHB', 'NSI'))
#' plot <- pr_plot_NRSEnvContour(df, Interpolation = TRUE, Fill_NA = FALSE, maxGap = 3)
pr_plot_NRSEnvContour <- function(df, Interpolation = TRUE, Fill_NA = FALSE, maxGap = 3) {

  stations <- unique(as.character(df$StationName))
  param <- planktonr::pr_relabel(unique(df$Parameters), style = 'ggplot')

  minDate <- lubridate::floor_date(min(df$SampleTime_Local, na.rm = TRUE), unit = 'month')

  df <- df %>%
    dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values),
                  SampleDepth_m = dplyr::case_when(.data$StationName %in% c('Darwin') ~ round(.data$SampleDepth_m/10, 0)*10,
                                                   TRUE ~ round(.data$SampleDepth_m/5, 0)*5)) %>%
    dplyr::mutate(MonthSince = lubridate::interval(minDate, .data$SampleTime_Local) %/% months(1)) %>%
    dplyr::mutate(Year = lubridate::year(.data$SampleTime_Local),
                  Label = ifelse(.data$Month_Local == 6, .data$Year, NA))

  if (Interpolation == FALSE) {

    myBreaks <- (df %>% dplyr::filter(!is.na(.data$Label)) %>% dplyr::distinct(.data$MonthSince) %>%
                   dplyr::arrange(dplyr::desc(-.data$MonthSince), .by_group = FALSE))$MonthSince
    Label <- (df %>% dplyr::group_by(.data$Label) %>% dplyr::summarise(n = dplyr::n()) %>% tidyr::drop_na() %>%
                dplyr::distinct(.data$Label))$Label

  } else {
    plotfunc <- function(stations) {
      df <- df %>% dplyr::filter(.data$StationName == stations) %>%
        dplyr::select("MonthSince", "SampleDepth_m", "Values")

      min <- min(df$MonthSince)
      Depths = unique(df$SampleDepth_m)
      Months <- seq(min(df$MonthSince), max(df$MonthSince), 1)

      emptyGrid <- expand.grid(SampleDepth_m = Depths,
                               MonthSince = Months)

      df <- emptyGrid %>%
        dplyr::left_join(df, by = c("MonthSince", "SampleDepth_m")) %>%
        data.frame() %>%
        dplyr::arrange(.data$MonthSince, .data$SampleDepth_m)

      mat <- df %>%
        tidyr::pivot_wider(names_from = "MonthSince", values_from = "Values", values_fn = mean) %>%
        dplyr::select(-"SampleDepth_m") %>%
        as.matrix.data.frame()

      if(Fill_NA == TRUE){
        mat <- t(zoo::na.approx(t(mat), maxgap = maxGap))
        mat <- zoo::na.approx(mat, maxgap = maxGap)
      }

      Months2 <- seq(0, length(Months)-1, 1)

      interped <- expand.grid(SampleDepth_m = Depths,
                              MonthSince = Months2)

      interp_vals <- pracma::interp2(y = seq(0, max(df$SampleDepth_m, na.rm = TRUE), length.out = nrow(mat)),
                                     x = seq(0, max(df$MonthSince), length.out = ncol(mat)),
                                     Z = mat,
                                     yp = interped$SampleDepth_m,
                                     xp = interped$MonthSince,
                                     method = "linear")

      dfInterp <- dplyr::bind_cols(interped, Values = interp_vals) %>% data.frame() %>%
        dplyr::mutate(StationName = stations,
                      Values = ifelse(.data$Values < 0, 0, .data$Values),
                      MonthSince = .data$MonthSince + min)

    }

    PlotData <- purrr::map_dfr(stations, plotfunc) # Interpolating across time and depth for the station

    df <- PlotData %>%
      dplyr::left_join(df %>% dplyr::select('MonthSince', 'SampleDepth_m', 'StationName', 'Label', 'Month_Local'),
                       by = c("MonthSince", "SampleDepth_m", "StationName")) %>%
      dplyr::distinct() %>%
      pr_reorder()

    Label <- (df %>% dplyr::group_by(.data$Label) %>% dplyr::summarise(n = dplyr::n()) %>% tidyr::drop_na() %>%
                dplyr::distinct(.data$Label))$Label

    myBreaks <- (df %>% dplyr::filter(!is.na(.data$Label)) %>% dplyr::distinct(.data$MonthSince) %>%
                   dplyr::arrange(dplyr::desc(-.data$MonthSince), .by_group = FALSE))$MonthSince
  }

  # Function for plots
  outPlot <- function(df, x, just = "left"){
    out <- ggplot2::ggplot(data = df, ggplot2::aes(x, y = .data$SampleDepth_m, z = .data$Values)) +
      ggplot2::geom_contour_filled(bins = 8) +
      ggplot2::facet_wrap(.data$StationName ~ ., scales = "free_y", ncol = 1, strip.position = "top") +
      theme_pr() +
      ggplot2::theme(legend.justification = just) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = param, title.position = 'top')) +
      ggplot2::scale_y_reverse(expand = c(0, 0))
    return(out)
  }

  # Plotting year time series
  out <- outPlot(df, df$MonthSince, "left") +
    ggplot2::labs(x = "Year", y = 'Depth (m)') +
    ggplot2::scale_x_continuous(breaks = myBreaks, labels = Label, expand = c(0, 0))

  if(Interpolation == FALSE){
    out <- out +
      ggplot2::geom_point(colour = 'dark grey', cex = 0.75)
  }

  # Prepare monthly climatology data

  if(Fill_NA == TRUE){
    selecs <- df %>%
      dplyr::select("MonthSince", "Month_Local") %>%
      dplyr::distinct() %>%
      tidyr::drop_na()

    df <- df %>%
      dplyr::select(-"Month_Local") %>%
      dplyr::left_join(selecs, by = c("MonthSince"))
  }

  dfMon <- df %>%
    dplyr::group_by(.data$Month_Local, .data$StationName, .data$SampleDepth_m) %>%
    dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                     .groups = 'drop')


  # Plotting monthly climatology
  outMon <- outPlot(dfMon, dfMon$Month_Local, "right") +
    ggplot2::labs(x = "Month") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   #strip.text = ggplot2::element_blank(),
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                                expand = c(0, 0))

  plots <- patchwork::wrap_plots(out, outMon, ncol = 2) +
    patchwork::plot_layout(widths = c(3,1), heights = length(stations) * 200)

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
#' df <- data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
#'                  freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
#'                  Season = c("December - February","March - May",
#'                  "June - August","September - November"),
#'                  Taxon = 'Acartia danae',
#'                  Survey = 'CPR')
#' plot <- pr_plot_FreqMap(df, species = 'Acartia danae', interactive = TRUE)
pr_plot_FreqMap <- function(df, species, interactive = TRUE){

  dfa <- df %>% dplyr::select('Season', 'Latitude', 'Longitude', 'Survey') %>%
    dplyr::distinct()

  dff <- df %>%
    dplyr::filter(.data$Taxon %in% species) %>%
    dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Seen in 25%",'50%', '75%', '100% of Samples'))) %>%
    dplyr::arrange(.data$freqfac)

  if(interactive == FALSE){
    cols <- c("lightblue1", "skyblue3", "blue1", "navyblue")

    Species <- unique(df$Taxon)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = MapOz) +
      ggplot2::geom_point(data=dfa, ggplot2::aes(x=.data$Longitude, y=.data$Latitude, pch = .data$Survey), colour='light grey', size = 1) +
      ggplot2::geom_point(data=dff, ggplot2::aes(x=.data$Longitude, y=.data$Latitude, colour=.data$freqfac, pch = .data$Survey), size = 2) +
      ggplot2::facet_wrap( ~ .data$Season, dir = "v") +
      ggplot2::labs(title = Species) +
      ggplot2::scale_colour_manual(name = "", values = cols, drop = FALSE) +
      theme_pr() +
      ggplot2::theme(title = ggplot2::element_text(face = "italic"),
                     # legend.title = ggplot2::element_text(face = "plain", size = 12),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "snow1"),
                     legend.key = ggplot2::element_blank())

    return(p)

  } else {

    df <- dff %>% dplyr::group_split(.data$Season)

    plotlist <- function(dflist){

      CPRpal <- leaflet::colorFactor(c("lightblue1", "skyblue3", "dodgerblue", "blue1", "navyblue"), domain = dflist$freqfac)
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
        leaflet::addCircleMarkers(data = dfa, group = 'All Samples',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  radius = 1,
                                  color = 'light grey',
                                  fill = 'light grey') %>%
        leaflet::addCircleMarkers(data = dfCPR, group = 'Continuous Plankton Recorder',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  radius = 5,
                                  color = ~CPRpal(freqfac),
                                  fill = ~CPRpal(freqfac)) %>%
        leaflet::addCircleMarkers(data = dfNRS , group = 'National Reference Stations',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  color = ~NRSpal(freqfac),
                                  fill = ~NRSpal(freqfac),
                                  radius = 5) %>%
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

#' Pie plots of functional groups for data
#'
#' @param df data frame binned to functional group e.g. planktonr::pr_get_FuncGroups("NRS", "Z")
#'
#' @return pie plot of functional groups
#' @export
#'
#' @examples
#' df <- pr_get_FuncGroups("CPR", "P")
#' plot <- pr_plot_PieFG(df)
#'
pr_plot_PieFG <- function(df){

  if('BioRegion' %in% colnames(df)){
    Survey = 'CPR'
  } else if ('StationCode' %in% colnames(df)){
    Survey = 'NRS'
  } else {
    Survey = ''
  }

  if(nrow(df %>% dplyr::filter(grepl('iatom', df$Parameters))) > 0){
    plotTitle = 'Phytoplankton'
  } else if (nrow(df %>% dplyr::filter(grepl('opepod', df$Parameters))) > 0){
    plotTitle = 'Zooplankton'
  } else {
    plotTitle = ''
  }

  p <- ggplot2::ggplot(data = df %>%
                         dplyr::group_by(.data$Parameters) %>%
                         dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE)),
                       ggplot2::aes(x = "", y = mean, fill = .data$Parameters)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() +  # remove background, grid, numeric labels
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = 'bottom') +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = plotTitle, nrow = 2, title.position = "top",
                                                 title.hjust = 0.5, title.theme = ggplot2::element_text(face = "bold"))) +
    ggplot2::ggtitle(Survey)

  return(p)
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
    ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), expand = c(0, 0),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    theme_pr() +
    ggplot2::labs(y = ylabel, x = "Month") +
    ggplot2::ggtitle(titlemain) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))

  return(plots)

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
                  density(SST,
                          weight = weight,
                          bw = kernBw,
                          from = kernMin,
                          to = kernMax,
                          n = kernN))

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
    theme_pr() +
    ggplot2::labs(x = xlabel, y = "Relative kernel density") +
    ggplot2::ggtitle(taxon, subtitle = subtit) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))

  return(stiplot)

}


#' IMOS progress plot
#'
#' @param df output from pr_get_ProgressMapData
#' @param interactive Should the plot be interactive with leaflet?
#' @param labels TRUE/FALSE Should labels be added to leaflet plot? Adding labels adds more information but slows down the rendering.
#'
#' @return a plot of IMOS progress
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMapData("NRS")
#' plot <- pr_plot_ProgressMap(df)
pr_plot_ProgressMap <- function(df, interactive = FALSE, labels = TRUE){

  if (interactive == TRUE){

    if (is.data.frame(df) == TRUE){ # planktonr will likely return a df
      df_CPR <- df %>%
        dplyr::filter(.data$Survey == "CPR" & (!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL)))

      df_PCI <- df %>%
        dplyr::filter(.data$Survey == "CPR" & is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))

      df_NRS <- df %>% dplyr::filter(.data$Survey == "NRS")
    } else if (is.data.frame(df) == FALSE){ # boo will return a list to shrink size

      df_CPR <- df$CPR %>%
        dplyr::filter((!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL)))

      df_PCI <- df$CPR %>%
        dplyr::filter(is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))

      df_NRS <- df$NRS

    }

    if(labels == TRUE){
      labs_cpr <- lapply(seq(nrow(df_CPR)), function(i) {
        paste("<strong>Sample Date:</strong>", df_CPR$SampleTime_Local[i], "<br>",
              "<strong>Bioregion:</strong>", df_CPR$Name[i], "<br>",
              "<strong>Latitude:</strong>", df_CPR$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", df_CPR$Longitude[i], "<br>",
              "<strong>Phytoplankton Abundance (L\u207B\u00B9)</strong>: ", round(df_CPR$PhytoAbundance_CellsL[i],2), "<br>",
              "<strong>Zooplankton Abundance (m\u207B\u00B3)</strong>: ", round(df_CPR$ZoopAbundance_m3[i],2), "<br>",
              "<strong>Phytoplankton Colour Index:</strong>", df_CPR$PCI[i], "<br>")})

      labs_pci <- lapply(seq(nrow(df_CPR)), function(i) {
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

      labs_mbr <- lapply(labs_mbr, htmltools::HTML)
      labs_cpr <- lapply(labs_cpr, htmltools::HTML)
      labs_pci <- lapply(labs_pci, htmltools::HTML)
    } else {
      labs_mbr <- NULL
      labs_cpr <- NULL
      labs_pci <- NULL
    }

    if (is.data.frame(df) == FALSE){
      labs_nrs <- lapply(seq(nrow(df_NRS)), function(i) {

        if (lubridate::year(df_NRS$End_Date[i]) < 2020){
          EndDate <- paste0(df_NRS$End_Date[i], " (discontinued)")
        } else {
          EndDate <- paste0(df_NRS$End_Date[i], " (ongoing)")
        }

        paste("<strong>National Reference Station:</strong>", df_NRS$Name[i], "<br>",
              "<strong>First Sampling:</strong>", df_NRS$Start_Date[i], "<br>",
              "<strong>Last Sampling:</strong>", EndDate, "<br>",
              "<strong>Latitude:</strong>", df_NRS$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", df_NRS$Longitude[i], "<br>",
              "<strong>Number of Sampling Trips:</strong>", df_NRS$Samples[i], "<br>")})
      labs_nrs <- lapply(labs_nrs, htmltools::HTML)
    } else if (is.data.frame(df) == TRUE){
      labs_nrs <- NULL
    }


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
                           label = labs_mbr
      ) %>%
      leaflet::addCircleMarkers(data = df_CPR,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 3, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (Phyto/Zoo Counts)",
                                label = labs_cpr,
      ) %>%
      leaflet::addCircleMarkers(data = df_PCI,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 1, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (PCI Only)",
                                label = labs_pci,
      ) %>%
      leaflet::addCircleMarkers(data = df_NRS,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                color = "#FFA500",
                                radius = 10, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "National Reference Stations",
                                label = labs_nrs,
      ) %>%
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
                         colors = mbr %>%
                           sf::st_drop_geometry() %>%
                           dplyr::distinct(.data$REGION, .keep_all = TRUE) %>%
                           dplyr::pull(.data$Colour),
                         labels = mbr %>%
                           sf::st_drop_geometry() %>%
                           dplyr::distinct(.data$REGION) %>%
                           dplyr::pull(.data$REGION),
                         title = "Bioregions",
                         opacity = 1) %>%
      leaflet::hideGroup("Continuous Plankton Recorder (PCI Only)")



    return(map)

  } else {

    MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                         returnclass = "sf")

    PMapData2 <- df %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

    PMapSum <- merge(df %>%
                       dplyr::group_by(.data$Region, .data$Survey) %>%
                       dplyr::summarise(Sums = dplyr::n(),
                                        .groups = "drop") %>%
                       dplyr::mutate(label = paste0(.data$Region, ' = ', .data$Sums)),
                     df %>%
                       dplyr::group_by(.data$Region, .data$Survey) %>%
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
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank())


    if ("CPR" %in% Survey$Survey) {
      gg <- gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "CPR"),
                         size = 1, ggplot2::aes(color =.data$Region), show.legend = TRUE) +
        ggplot2::theme(legend.position = c(.01, .99),
                       legend.justification = c("left", "top"),
                       legend.box.just = "left",
                       legend.background = ggplot2::element_rect(fill = "transparent", colour = "NA"), #transparent legend bg
                       legend.box.background = ggplot2::element_rect(fill = "transparent", colour = "NA")) #transparent legend panel) #+
      # ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "CPR"),
      #                       size = 5, ggplot2::aes(label = .data$label, color = .data$Region),
      #                       show.legend = FALSE, check_overlap = TRUE, nudge_x = nudgex, nudge_y = nudgey)
    }

    if ("NRS" %in% Survey$Survey) {
      gg <- gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "NRS"), size = 3) +
        ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "NRS"),
                              size = 4, ggplot2::aes(label = .data$label),
                              show.legend = FALSE, nudge_y = -1)
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
      theme_pr() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("Continuous Plankton Recorder Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return (gg)


  } else if (Survey == "NRS"){

    dat2 <- dat %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local/12) %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      dplyr::group_by(.data$YearMonth, .data$StationName) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$StationName, width = 1/12, height = 2/12), fill = "black", colour = "black") +
      ggplot2::geom_tile() +
      theme_pr() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("National Reference Station Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return(gg)

  }

}



#' Taxa Accumulation Curve
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

  gg <- ggplot2::ggplot(data = dat, ggplot2::aes(x = .data$First, y = .data$RowN)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = "Year", breaks = "2 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::ylab("Taxa Identified") +
    theme_pr() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle(paste(Survey, "-", planktonr::pr_title(Type)))

  return(gg)

}

#' Simple function to scatter 2 data columns using common NRS colouring
#'
#' Note that this function assumes wide data with the data to plot as columns.
#'
#' @param df Dataframe
#' @param x Column name for the x axis
#' @param y Column name for the y axis
#' @param Trend Trend line through scatter plot
#'
#' @return ggplot object
#' @export

#' @examples
#' df <- planktonr::pr_get_NRSMicro() %>%
#' tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
#' dplyr::filter(.data$StationCode %in% c("NSI", "PHB")) %>%
#' tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = 'mean')
#' gg <- pr_plot_scatter(df, "Bacterial_Temperature_Index_KD",
#' "nitrogen_fixation_organisms", Trend = 'none')

pr_plot_scatter <- function(df, x, y, Trend = 'none'){

  if("BioRegion" %in% colnames(df)){
    cols <- colCPR
    pchs <- pchCPR
    gg <-  ggplot2::ggplot(data = df, ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y), colour = .data$BioRegion, pch = .data$BioRegion))
    aesSN <- ggplot2::aes(fill = .data$BioRegion)
    } else {
    cols <- colNRSName
    pchs <- pchNRSName
    gg <-  ggplot2::ggplot(data = df, ggplot2::aes(!!rlang::sym(x), !!rlang::sym(y), colour = .data$StationName, pch = .data$StationName))
    aesSN <- ggplot2::aes(fill = .data$StationName)
  }

  titlex <- planktonr::pr_relabel(x, style = "ggplot")
  titley <- planktonr::pr_relabel(y, style = "ggplot")

  gg <-  gg +
    ggplot2::geom_point() +
    ggplot2::xlab(titlex) +
    ggplot2::ylab(titley) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_shape_manual(values = pchs) +
    planktonr::theme_pr()

  if("SampleDepth_m" %in% colnames(df)){
    gg <- gg + ggplot2::facet_grid(.data$SampleDepth_m ~ ., scales = "free_y") +
      ggplot2::theme(strip.text.y = ggplot2::element_text(face = "bold", angle = 0)) # size = 12
  }

  if(Trend == 'Linear'){
    gg <- gg + ggplot2::geom_smooth(method = 'lm', formula = 'y ~ x', aesSN, alpha = 0.2) +
      ggplot2::scale_fill_manual(values = cols)
  }

  if(Trend == 'Smoother'){
    gg <- gg + ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x', aesSN, alpha = 0.2) +
      ggplot2::scale_fill_manual(values = cols)
  }


  return(gg)
}

#' Simple boxplot function using common NRS colouring
#'
#' Note that this function assumes wide data with the data to plot as columns.
#'
#' @param df Dataframe
#' @param y Column name for the y axis
#'
#' @return ggplot object
#' @export

#' @examples
#' df <- planktonr::pr_get_NRSMicro('Coastal') %>%
#' tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
#' dplyr::filter(.data$StationCode %in% c("DEE", "DEB")) %>%
#' tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = 'mean')
#' gg <- pr_plot_box(df, "Bacterial_Temperature_Index_KD")

pr_plot_box <- function(df, y){

  if("BioRegion" %in% colnames(df)){
    cols <- colCPR
    pchs <- pchCPR
    ltys <- ltyCPR
    gg <- ggplot2::ggplot(data = df,
                          ggplot2::aes(.data$BioRegion, !!rlang::sym(y), color = .data$BioRegion, linetype = .data$BioRegion))
  } else {
    cols <- colNRSName
    pchs <- pchNRSName
    ltys <- ltyNRSName
    gg <- ggplot2::ggplot(data = df,
                          ggplot2::aes(.data$StationName, !!rlang::sym(y), color = .data$StationName, linetype = .data$StationName))
  }

  titley <- planktonr::pr_relabel(y, style = "ggplot")

  gg <- gg +
    ggplot2::geom_point() +
    ggplot2::geom_boxplot() +
    ggplot2::ylab(titley) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_linetype_manual(values = ltys)  +
    ggplot2::scale_shape_manual(values = pchs) +
    planktonr::theme_pr()

  return(gg)
}


