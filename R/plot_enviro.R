
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
#' df <- pr_get_NRSEnvContour("Chemistry") %>%
#'   dplyr::filter(Parameters == "Nitrate_umolL",
#'   StationCode %in% c('YON', 'MAI', 'PHB', 'NSI'))
#' plot <- pr_plot_NRSEnvContour(df, Interpolation = TRUE, Fill_NA = FALSE, maxGap = 3)
#'
pr_plot_NRSEnvContour <- function(df, Interpolation = TRUE, Fill_NA = FALSE, maxGap = 3) {

  if (isTRUE(Interpolation)){
    rlang::check_installed("pracma", reason = "to use `interp2()`")
    # code that includes calls such as aaapkg::aaa_fun()
  }

  Type <- pr_get_type(df)
  Survey <- pr_get_survey(df)

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
    Label <- (df %>% dplyr::summarise(n = dplyr::n(), .by = tidyselect::all_of("Label")) %>% tidyr::drop_na() %>%
                dplyr::distinct(.data$Label))$Label

  } else {
    plotfunc <- function(stations) {
      df <- df %>% dplyr::filter(.data$StationName == stations) %>%
        dplyr::select(tidyselect::all_of(c("MonthSince", "SampleDepth_m", "Values")))

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
        dplyr::select(-tidyselect::all_of("SampleDepth_m")) %>%
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

    df <- purrr::map_dfr(stations, plotfunc) %>% # Interpolating across time and depth for the station
      planktonr_dat(Type = Type, Survey = Survey) %>%
      dplyr::left_join(df %>% dplyr::select(tidyselect::all_of(c("MonthSince", "SampleDepth_m", "StationName", "Label", "Month_Local"))),
                       by = c("MonthSince", "SampleDepth_m", "StationName")) %>%
      dplyr::distinct() %>%
      pr_reorder()

    Label <- (df %>% dplyr::summarise(n = dplyr::n(), .by = tidyselect::all_of("Label")) %>% tidyr::drop_na() %>%
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
      dplyr::select(tidyselect::all_of(c("MonthSince", "Month_Local"))) %>%
      dplyr::distinct() %>%
      tidyr::drop_na()

    df <- df %>%
      dplyr::select(-tidyselect::all_of("Month_Local")) %>%
      dplyr::left_join(selecs, by = c("MonthSince"))
  }

  dfMon <- df %>%
    dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                     .by = tidyselect::all_of(c("Month_Local", "StationName", "SampleDepth_m")))


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
