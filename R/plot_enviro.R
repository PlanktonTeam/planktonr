
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
#' plot <- pr_plot_NRSEnvContour(df, Interpolation = FALSE)
pr_plot_NRSEnvContour <- function(df, Interpolation = TRUE) {

  # if (isTRUE(Interpolation)){
  #   rlang::check_installed("metR", reason = "to use `geom_contour_fill()`")
  #   # code that includes calls such as aaapkg::aaa_fun()
  # }

  df <- df %>%
    mutate(SampleDepth_m = round(.data$SampleDepth_m/10, 0)*10,  ## leave in for SOTS
           MonthSince = lubridate::interval(min(.data$SampleTime_Local), .data$SampleTime_Local) %/% months(1)) %>%
    filter(SampleDepth_m < 600) ## leave in for SOTS

  # data for timeseries
  dfts <- df %>%
    group_by(SampleDepth_m, MonthSince, Parameters, StationName) %>%
    summarise(Values = mean(Values, na.rm = TRUE),
              .groups = 'drop')

  #data for climatology
  dfMon <- df %>%
    dplyr::group_by(Month_Local, StationName, SampleDepth_m) %>%
    dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                     .groups = 'drop')

  # breaks and legend titles for timeseries and climatology

  titleg <- pr_relabel(unique(df$Parameters), style = "ggplot")
  limitMin <- min(dfts$Values, na.rm = TRUE)
  limitMax <- max(dfts$Values, na.rm = TRUE)

  ## Using metR
  plotting <- function(plt){
    if(plt == 'ts'){
      minDate <- min(df$SampleTime_Local)
      maxMonths <- max(lubridate::interval(min(df$SampleTime_Local), df$SampleTime_Local) %/% months(1))
      myBreaks <- seq(9, maxMonths, 24)
      myLabels <- year(minDate %m+% months(myBreaks))
      df <- dfts
      xvals <- "MonthSince"
    } else {
      myBreaks <- seq(1, 12, length.out = 12)
      myLabels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
      df <- dfMon
      xvals <- "Month_Local"
    }

    if(Interpolation == TRUE){
      p1 <- ggplot2::ggplot() +
        metR::geom_contour_fill(data = df, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m, z = .data$Values), na.fill = TRUE) +
        ggplot2::scale_fill_continuous(type = "viridis", name = titleg, limits = c(limitMin, limitMax))+
        ggplot2::guides(fill = guide_colourbar(barwidth = 4, barheight = 1))
    } else{
      p1 <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m, z = .data$Values)) +
        ggplot2::geom_contour_filled(bins = 8) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = titleg))
    }

    p1 <- p1 +
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

  plots <- patchwork::wrap_plots(pts, pmc, ncol = 2) +
    patchwork::plot_layout(widths = c(3,1))

  return(plots)

}
