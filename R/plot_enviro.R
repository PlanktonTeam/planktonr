
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
#' df <- pr_get_NRSChemistry() %>% dplyr::filter(Parameters == "SecchiDepth_m",
#' StationCode %in% c('PHB', 'NSI'))
#' pr_plot_Enviro(df)
#'
pr_plot_Enviro <- function(df, Trend = "None", trans = "identity") {

  # Input validation
  assertthat::assert_that(
    is.data.frame(df),
    msg = "'df' must be a data frame."
  )
  
  assertthat::assert_that(
    nrow(df) > 0,
    msg = "The data frame 'df' is empty. Check your data source or filtering criteria."
  )
  
  assertthat::assert_that(
    is.character(Trend) && length(Trend) == 1,
    msg = "'Trend' must be a single character string. Valid options are 'None', 'Smoother', or 'Linear'."
  )
  
  assertthat::assert_that(
    Trend %in% c("None", "Smoother", "Linear"),
    msg = "'Trend' must be one of 'None', 'Smoother', or 'Linear'."
  )
  
  assertthat::assert_that(
    is.character(trans) && length(trans) == 1,
    msg = "'trans' must be a single character string specifying the y-axis transformation (e.g., 'identity', 'log10', 'sqrt')."
  )
  
  # Check required columns
  required_cols <- c("SampleTime_Local", "Values", "Parameters", "StationName", "SampleDepth_m", "Month_Local")
  assertthat::assert_that(
    all(required_cols %in% names(df)),
    msg = paste0("'df' must contain the following columns: ", paste(required_cols, collapse = ", "), ".")
  )

  n <- length(unique(df$StationName))

  titley <- pr_relabel(unique(df$Parameters), style = "ggplot")

  np <- length(unique(df$SampleDepth_m))

  df <- df %>%
    dplyr::mutate(SampleDepth_ms = stringr::str_c(.data$SampleDepth_m," m"))

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleTime_Local, .data$Values, colour = .data$StationName,
                                        fill = .data$StationName, linetype = .data$StationName)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Year", y = titley) +
    ggplot2::facet_wrap(.data$SampleDepth_ms ~., scales = "free_y", ncol = 1, strip.position = "right") +
    theme_pr() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_colour_manual(values = colNRSName, limits = force) +
    ggplot2::scale_fill_manual(values = colNRSName, limits = force) +
    ggplot2::scale_linetype_manual(values = ltyNRSName)

  if(Trend == "Smoother"){
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, alpha = 0.2)
  }
  if(Trend == "Linear"){
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
  }

  mdat <- df %>%
    dplyr::summarise(MonValues = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .by = tidyselect::all_of(c("StationName", "Month_Local", "SampleDepth_ms", "Parameters")))

  m <- ggplot2::ggplot(mdat, ggplot2::aes(.data$Month_Local, .data$MonValues, colour = .data$StationName,
                                          fill = .data$StationName, linetype = .data$StationName)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(.data$SampleDepth_ms ~., scales = "free_y", ncol = 1, strip.position = "right") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, alpha = 0.2) +
    ggplot2::scale_x_continuous(breaks = seq(1,12,length.out = 12), expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    ggplot2::scale_colour_manual(values = colNRSName, limits = force) +
    ggplot2::scale_fill_manual(values = colNRSName, limits = force) +
    ggplot2::scale_linetype_manual(values = ltyNRSName) +
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
#' @param na.fill TRUE, FALSE or function like mean to fill in gaps in data
#'
#' @return a contour plot
#' @export
#'
#' @examples
#' df <- pr_get_NRSEnvContour("Chemistry") %>% dplyr::filter(Parameters == "Nitrate_umolL",
#' StationCode %in% c('YON', 'MAI', 'PHB', 'NSI'))
#' plot <- pr_plot_NRSEnvContour(df, na.fill = TRUE)
pr_plot_NRSEnvContour <- function(df, na.fill = TRUE) {

  # Input validation
  assertthat::assert_that(
    is.data.frame(df),
    msg = "'df' must be a data frame."
  )
  
  assertthat::assert_that(
    nrow(df) > 0,
    msg = "The data frame 'df' is empty. Check your data source or filtering criteria."
  )
  
  assertthat::assert_that(
    is.logical(na.fill) || is.function(na.fill),
    msg = "'na.fill' must be TRUE, FALSE, or a function (e.g., mean) to fill in gaps in data."
  )
  
  # Check required columns
  required_cols <- c("SampleTime_Local", "Values", "Parameters", "SampleDepth_m")
  assertthat::assert_that(
    all(required_cols %in% names(df)),
    msg = paste0("'df' must contain the following columns: ", paste(required_cols, collapse = ", "), ".")
  )

  df <- df %>%
    dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10, 0)*10,  ## leave in for SOTS
           MonthSince = lubridate::interval(min(.data$SampleTime_Local), .data$SampleTime_Local) %/% months(1)) %>%
    dplyr::filter(.data$SampleDepth_m < 600) ## leave in for SOTS

  # breaks and legend titles for timeseries and climatology

  titleg <- planktonr::pr_relabel(unique(df$Parameters), style = "ggplot")
  limitMin <- min(df$Values, na.rm = TRUE)
  limitMax <- max(df$Values, na.rm = TRUE)

  ## Using metR
  plotting <- function(plt){
    if(plt == 'ts'){
      minDate <- min(df$SampleTime_Local)
      maxMonths <- max(lubridate::interval(min(df$SampleTime_Local), df$SampleTime_Local) %/% months(1))
      myBreaks <- seq(9, maxMonths, 24)
      myLabels <- lubridate::year(minDate %m+% months(myBreaks))
      xvals <- "MonthSince"
    } else {
      myBreaks <- seq(1, 12, length.out = 12)
      myLabels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
      xvals <- "Month_Local"
    }

    df <- df %>%
      dplyr::group_by(!!rlang::sym(xvals), .data$StationName, .data$SampleDepth_m) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = 'drop')

      p1 <- ggplot2::ggplot() +
        metR::geom_contour_fill(data = df, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m, z = .data$Values), na.fill = na.fill) +
        ggplot2::scale_fill_continuous(type = "viridis", name = titleg, limits = c(limitMin, limitMax))+
        ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = grid::unit(0.3, "npc"), barheight = grid::unit(0.04, "npc"))) +
        ggplot2::geom_point(data = df, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m), size = 1) +
        ggplot2::facet_wrap(~.data$StationName, scales = "free_y", ncol = 1) +
        ggplot2::scale_x_continuous(breaks = myBreaks, labels = myLabels, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        planktonr::theme_pr() +
        ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0),
                     legend.position = 'bottom')

    return(p1)

  }

  pts <- plotting(plt = 'ts') +
    ggplot2::labs(x = "Year", y = "Depth (m)")

  pmc <-  plotting(plt = 'mc') +
    ggplot2::labs(x = "Month") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())

  plots <- patchwork::wrap_plots(pts, pmc, ncol = 2, guides = "collect") +
    patchwork::plot_layout(widths = c(3,1)) +
    patchwork::plot_annotation(theme = ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ))


  return(plots)

}
