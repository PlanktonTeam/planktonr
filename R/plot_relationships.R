
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
#' dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
#' tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean)
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
#' dplyr::filter(StationCode %in% c("DEE", "DEB")) %>%
#' tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean)
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

