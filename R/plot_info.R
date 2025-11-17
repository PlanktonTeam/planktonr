

#' Plot Gantt Chart showing plankton sampling status
#'
#' @param dat Trip data for either NRS or CPR.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dat <- pr_get_CPRTrips()
#' gg <- pr_plot_Gantt(dat)
#' dat <- pr_get_NRSTrips()
#' gg <- pr_plot_Gantt(dat)
pr_plot_Gantt <- function(dat){

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame. Use pr_get_CPRTrips() or pr_get_NRSTrips() to create the data."
  )

  Survey <- pr_get_survey(dat)

  if (Survey == "CPR"){
    dat2 <- dat %>%
      dplyr::arrange(.data$Latitude) %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local/12) %>%
      dplyr::distinct(.data$YearMonth, .data$Region, .data$TripCode) %>%
      dplyr::summarise(n = dplyr::n(), .by = tidyselect::all_of(c("YearMonth", "Region", "TripCode")))

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$Region, width = 1/12, height = 2/12)) +
      ggplot2::geom_tile(fill = "black", colour = "black") +
      theme_pr() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("Continuous Plankton Recorder Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return (gg)


  } else if (Survey == "NRS"){

    dat2 <- dat %>%
      dplyr::mutate(YearMonth = .data$Year_Local + .data$Month_Local/12) %>%
      dplyr::filter(.data$StationName != "Port Hacking 4") %>%
      dplyr::summarise(n = dplyr::n(), .by = tidyselect::all_of(c("YearMonth", "StationName")))

    gg <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$YearMonth, y = .data$StationName, width = 1/12, height = 2/12)) +
      ggplot2::geom_tile(fill = "black", colour = "black") +
      theme_pr() +
      ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
      ggplot2::ggtitle("National Reference Station Sampling") +
      ggplot2::coord_fixed(ratio = 0.5)

    return(gg)

  }

}



#' Plot taxonomic accumulation curves showing cumulative species discovery over time
#'
#' Plot a taxa accumulation curve for everything that is identified by the IMOS plankton team
#'
#' @param dat A dataframe of plankton data
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' dat <- pr_get_TaxaAccum(Survey = "NRS", Type = "Zooplankton")
#' p <- pr_plot_TaxaAccum(dat)
#' dat <- pr_get_TaxaAccum(Survey = "CPR", Type = "Phytoplankton")
#' p <- pr_plot_TaxaAccum(dat)
pr_plot_TaxaAccum <- function(dat){

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame. Use pr_get_TaxaAccum() to create the data."
  )

  required_cols <- c("First", "RowN")
  assertthat::assert_that(
    all(required_cols %in% colnames(dat)),
    msg = paste0("'dat' must contain the following columns: ", paste(required_cols, collapse = ", "), ". Use pr_get_TaxaAccum() to create the data.")
  )

  Survey = pr_get_survey(dat)
  Type = pr_get_type(dat)

  gg <- ggplot2::ggplot(data = dat, ggplot2::aes(x = .data$First, y = .data$RowN)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = "Year", breaks = "2 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::ylab("Taxa Identified") +
    theme_pr() +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle(paste(Survey, "-", planktonr::pr_title(Type)))

  return(gg)

}




#' Create pie charts showing functional group composition
#'
#' Visualise the relative proportion of plankton functional groups averaged
#' across all samples. Useful for summarising community composition in a simple,
#' accessible format.
#'
#' @param df A dataframe from [pr_get_FuncGroups()] containing functional group
#'   abundance or biomass data
#'
#' @details
#' ## Plot Structure
#' The pie chart shows:
#' * Each functional group as a wedge
#' * Wedge size proportional to mean abundance/biomass across all samples
#' * Colours from the "Set1" palette for clear distinction
#' * Legend below plot listing all functional groups
#'
#' ## Interpretation
#' This plot provides a quick overview of which functional groups dominate the
#' plankton community on average. Use this for:
#' * Initial data exploration
#' * Comparing overall community structure between surveys or regions
#' * Educational presentations requiring simple visualisations
#'
#' ## Limitations
#' * Shows average composition only, hiding temporal variability
#' * Cannot show changes over time (use [pr_plot_tsfg()] for that)
#' * Works best with 5-10 functional groups; too many makes wedges hard to distinguish
#'
#' ## Functional Groups
#' The plot automatically detects whether data are:
#' * **Phytoplankton**: Diatoms, dinoflagellates, ciliates, etc.
#' * **Zooplankton**: Copepods, appendicularians, fish larvae, etc.
#'
#' And labels the legend accordingly.
#'
#' @return A ggplot2 object that can be further customised or saved with `ggsave()`
#'
#' @seealso
#' * [pr_get_FuncGroups()] to generate input data
#' * [pr_plot_tsfg()] for time series of functional group composition
#'
#' @export
#'
#' @examples
#' # Phytoplankton functional groups from CPR
#' df <- pr_get_FuncGroups("CPR", "Phytoplankton")
#' plot <- pr_plot_PieFG(df)
#' print(plot)
#'
#' # Zooplankton functional groups from NRS
#' df <- pr_get_FuncGroups("NRS", "Zooplankton")
#' pr_plot_PieFG(df)
#'
pr_plot_PieFG <- function(df){

  # Input validation
  assertthat::assert_that(
    inherits(df, "planktonr_dat"),
    msg = "'df' must be a planktonr_dat object. Use pr_get_FuncGroups() to create the data."
  )

  assertthat::assert_that(
    "Parameters" %in% colnames(df) && "Values" %in% colnames(df),
    msg = "'df' must contain 'Parameters' and 'Values' columns. Use pr_get_FuncGroups() to create the data."
  )

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
