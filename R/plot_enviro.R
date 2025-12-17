
#' Plot environmental data with depth profiles
#'
#' Create a two-panel figure showing environmental variables (nutrients, pigments,
#' picophytoplankton) as both time series and climatology plots, with separate lines
#' for different sampling depths. This is useful for visualising vertical structure
#' and temporal patterns in water column properties.
#'
#' @param dat A dataframe from [pr_get_NRSChemistry()], [pr_get_NRSPigments()],
#'   or [pr_get_NRSPico()] containing environmental data with depth information
#' @param Trend Type of trend line to add:
#'   * `"None"` - No trend line
#'   * `"Smoother"` - LOESS smooth (default, good for non-linear patterns)
#'   * `"Linear"` - Linear regression line
#' @param trans Transformation for the y-axis scale:
#'   * `"identity"` - No transformation (default)
#'   * `"log10"` - Log base 10 transformation (useful for pigments, nutrients)
#'   * `"sqrt"` - Square root transformation
#'   * Any other transformation accepted by [ggplot2::scale_y_continuous()]
#'
#' @details
#' This function creates a two-panel figure:
#'
#' ## Panel 1: Time Series (left, wider)
#' Shows the full time series with different colours/line types for each depth.
#' Useful for identifying:
#' * Long-term trends at different depths
#' * Episodic events (e.g., upwelling, mixing)
#' * Vertical stratification patterns
#' * Deep chlorophyll maximum dynamics
#'
#' ## Panel 2: Monthly Climatology (right, narrower)
#' Shows the mean seasonal cycle at each depth. Useful for identifying:
#' * Typical seasonal patterns (e.g., spring bloom, summer stratification)
#' * Depth of maximum values by season
#' * Seasonal vertical migration of features
#'
#' The function automatically rounds depth values and creates appropriate legends.
#' Use [pr_remove_outliers()] before plotting if extreme values are present.
#'
#' @return A patchwork object containing two ggplot2 panels side-by-side
#'
#' @seealso [pr_get_NRSChemistry()] for nutrient data,
#'   [pr_get_NRSPigments()] for pigment data,
#'   [pr_get_NRSPico()] for picophytoplankton data,
#'   [pr_plot_NRSEnvContour()] for contour plot visualisation
#'
#' @export
#'
#' @examples
#' # Plot total chlorophyll a with depth
#' dat <- pr_get_NRSPigments(Format = "binned") %>%
#'   pr_remove_outliers(2) %>%
#'   dplyr::filter(Parameters == "TotalChla",
#'                 StationCode %in% c("NSI", "MAI"))
#' pr_plot_Enviro(dat, Trend = "Smoother", trans = "log10")
#'
#' # Plot nitrate concentrations
#' dat <- pr_get_NRSChemistry() %>%
#'   dplyr::filter(Parameters == "Nitrate_umolL",
#'                 StationCode == "PHB")
#' pr_plot_Enviro(dat, Trend = "Linear", trans = "identity")
#'
#' # Plot Prochlorococcus abundance
#' dat <- pr_get_NRSPico() %>%
#'   dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
#'                 StationCode == "NSI")
#' pr_plot_Enviro(dat, Trend = "Smoother", trans = "log10")
#'
#' dat <- pr_get_NRSChemistry() %>% dplyr::filter(Parameters == "SecchiDepth_m",
#' StationCode %in% c('PHB', 'NSI'))
#' pr_plot_Enviro(dat)
#'
pr_plot_Enviro <- function(dat, Trend = "None", trans = "identity") {

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame."
  )

  assertthat::assert_that(
    nrow(dat) > 0,
    msg = "The data frame 'dat' is empty. Check your data source or filtering criteria."
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
    all(required_cols %in% names(dat)),
    msg = paste0("'dat' must contain the following columns: ", paste(required_cols, collapse = ", "), ".")
  )

  n <- length(unique(dat$StationName))

  titley <- pr_relabel(unique(dat$Parameters), style = "ggplot")

  np <- length(unique(dat$SampleDepth_m))

  dat <- dat %>%
    dplyr::mutate(SampleDepth_ms = stringr::str_c(.data$SampleDepth_m," m"))

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$SampleTime_Local, .data$Values, colour = .data$StationName,
                                        fill = .data$StationName, linetype = .data$StationName)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Year", y = titley) +
    ggplot2::facet_wrap(.data$SampleDepth_ms ~., scales = "free_y", ncol = 1, strip.position = "right") +
    ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_colour_manual(values = colNRSName, limits = force, name = "Station Name") +
    ggplot2::scale_fill_manual(values = colNRSName, limits = force, name = "Station Name") +
    ggplot2::scale_linetype_manual(values = ltyNRSName, name = "Station Name") +
    theme_pr() +
    ggplot2::theme(strip.text = ggplot2::element_blank())


  if(Trend == "Smoother"){
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, alpha = 0.2)
  }
  if(Trend == "Linear"){
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
  }

  mdat <- dat %>%
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
    m + patchwork::plot_layout(widths = c(3,1), heights = np * 200, guides = "collect") &
    patchwork::plot_annotation(theme = ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"))

  return(plots)

}


#' Plot environmental contours showing depth-time patterns
#'
#' Create contour plots showing how environmental variables change with both time
#' (x-axis) and depth (y-axis). This visualisation is particularly effective for
#' identifying vertical structure, stratification, and the deep chlorophyll maximum.
#'
#' @param dat A dataframe from [pr_get_NRSEnvContour()] containing environmental
#'   data formatted for contour plotting
#' @param na.fill How to handle missing data (gaps) in the contour:
#'   * `TRUE` - Fill gaps using linear interpolation (default, creates smoother contours)
#'   * `FALSE` - Leave gaps as-is (shows only measured data)
#'   * A function - Use custom interpolation (e.g., `mean`, `median`)
#'
#' @details
#' Contour plots are excellent for visualising:
#'
#' ## Vertical Structure
#' * Deep chlorophyll maximum (DCM) depth and intensity
#' * Nutricline depth and strength
#' * Stratification patterns (steep vs. gradual gradients)
#' * Surface vs. subsurface maxima
#'
#' ## Temporal Patterns
#' * Seasonal shoaling/deepening of features
#' * Upwelling events (nutrient-rich water at surface)
#' * Mixing events (homogenisation of the water column)
#' * Long-term changes in vertical structure
#'
#' ## Gap Filling
#' When `na.fill = TRUE`, the function uses `metR::geom_contour_fill()` with
#' linear interpolation to create smooth contours. This is appropriate for
#' environmental data with regular sampling patterns and small gaps. For irregular
#' sampling or large gaps, consider setting `na.fill = FALSE` to show only measured
#' data.
#'
#' The function automatically creates one facet per station and parameter combination.
#' Contours are filled with a colour scale, and contour lines are overlaid in grey.
#'
#' @return A ggplot2 object with contour plots faceted by station and parameter
#'
#' @seealso [pr_get_NRSEnvContour()] for preparing the input data,
#'   [pr_plot_Enviro()] for alternative line plot visualisation,
#'   [pr_get_NRSChemistry()], [pr_get_NRSPigments()], [pr_get_NRSPico()] for data sources
#'
#' @export
#'
#' @examples
#' dat <- pr_get_NRSEnvContour("Chemistry") %>% dplyr::filter(Parameters == "Nitrate_umolL",
#' StationCode %in% c('YON', 'MAI', 'PHB', 'NSI'))
#' plot <- pr_plot_NRSEnvContour(dat, na.fill = TRUE)
pr_plot_NRSEnvContour <- function(dat, na.fill = TRUE) {

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame."
  )

  assertthat::assert_that(
    nrow(dat) > 0,
    msg = "The data frame 'dat' is empty. Check your data source or filtering criteria."
  )

  assertthat::assert_that(
    is.logical(na.fill) || is.function(na.fill),
    msg = "'na.fill' must be TRUE, FALSE, or a function (e.g., mean) to fill in gaps in data."
  )

  # Check required columns
  required_cols <- c("SampleTime_Local", "Values", "Parameters", "SampleDepth_m")
  assertthat::assert_that(
    all(required_cols %in% names(dat)),
    msg = paste0("'dat' must contain the following columns: ", paste(required_cols, collapse = ", "), ".")
  )

  dat <- dat %>%
    dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10, 0)*10,  ## leave in for SOTS
           MonthSince = lubridate::interval(min(.data$SampleTime_Local), .data$SampleTime_Local) %/% months(1)) %>%
    dplyr::filter(.data$SampleDepth_m < 600) ## leave in for SOTS

  # breaks and legend titles for timeseries and climatology

  titleg <- pr_relabel(unique(dat$Parameters), style = "ggplot")
  limitMin <- min(dat$Values, na.rm = TRUE)
  limitMax <- max(dat$Values, na.rm = TRUE)

  ## Using metR
  plotting <- function(plt){
    if(plt == 'ts'){
      minDate <- min(dat$SampleTime_Local)
      maxMonths <- max(lubridate::interval(min(dat$SampleTime_Local), dat$SampleTime_Local) %/% months(1))
      myBreaks <- seq(9, maxMonths, 24)
      myLabels <- lubridate::year(minDate %m+% months(myBreaks))
      xvals <- "MonthSince"
    } else {
      myBreaks <- seq(1, 12, length.out = 12)
      myLabels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
      xvals <- "Month_Local"
    }

    dat <- dat %>%
      dplyr::group_by(!!rlang::sym(xvals), .data$StationName, .data$SampleDepth_m) %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .groups = 'drop')

      p1 <- ggplot2::ggplot() +
        metR::geom_contour_fill(data = dat, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m, z = .data$Values), na.fill = na.fill) +
        ggplot2::scale_fill_continuous(type = "viridis", name = titleg, limits = c(limitMin, limitMax))+
        ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = grid::unit(0.3, "npc"), barheight = grid::unit(0.04, "npc"))) +
        ggplot2::geom_point(data = dat, ggplot2::aes(x = !!rlang::sym(xvals), y = .data$SampleDepth_m), size = 1) +
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
