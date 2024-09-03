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
pr_plot_Gantt <- function(dat, Survey = "NRS") {
  if (Survey == "CPR") {
    dat2 <- dat %>%
      dplyr::arrange(.data$Latitude) %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local / 12) %>%
      dplyr::distinct(.data$YearMonth, .data$Region, .data$TripCode) %>%
      dplyr::group_by(.data$YearMonth, .data$Region, .data$TripCode) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup()

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$Region, width = 1 / 12, height = 2 / 12), fill = "black", colour = "black") +
      ggplot2::geom_tile() +
      theme_pr() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("Continuous Plankton Recorder Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return(gg)
  } else if (Survey == "NRS") {
    dat2 <- dat %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local / 12) %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      dplyr::group_by(.data$YearMonth, .data$StationName) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$StationName, width = 1 / 12, height = 2 / 12), fill = "black", colour = "black") +
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
pr_plot_TaxaAccum <- function(dat, Survey = "NRS", Type = "Z") {
  gg <- ggplot2::ggplot(data = dat, ggplot2::aes(x = .data$First, y = .data$RowN)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = "Year", breaks = "2 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::ylab("Taxa Identified") +
    theme_pr() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle(paste(Survey, "-", planktonr::pr_title(Type)))

  return(gg)
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
pr_plot_PieFG <- function(df) {
  if ("BioRegion" %in% colnames(df)) {
    Survey <- "CPR"
  } else if ("StationCode" %in% colnames(df)) {
    Survey <- "NRS"
  } else {
    Survey <- ""
  }

  if (nrow(df %>% dplyr::filter(grepl("iatom", df$Parameters))) > 0) {
    plotTitle <- "Phytoplankton"
  } else if (nrow(df %>% dplyr::filter(grepl("opepod", df$Parameters))) > 0) {
    plotTitle <- "Zooplankton"
  } else {
    plotTitle <- ""
  }

  p <- ggplot2::ggplot(
    data = df %>%
      dplyr::group_by(.data$Parameters) %>%
      dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE)),
    ggplot2::aes(x = "", y = mean, fill = .data$Parameters)
  ) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() + # remove background, grid, numeric labels
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = plotTitle, nrow = 2, title.position = "top",
      title.hjust = 0.5, title.theme = ggplot2::element_text(face = "bold")
    )) +
    ggplot2::ggtitle(Survey)

  return(p)
}
