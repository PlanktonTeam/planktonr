

#' Plot basic timeseries
#'
#' @param df dataframe with SampleDate_Local, station code and parameter name and values
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a timeseries plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices("NRS", "Zooplankton") %>%
#'   dplyr::filter(Parameters == "Biomass_mgm3", StationCode %in% c("NSI", "PHB"))
#' timeseries <- pr_plot_TimeSeries(df)

pr_plot_TimeSeries <- function(df, trans = "identity"){

  Survey <- pr_get_survey(df)

  if (Survey == "CPR"){
    df <- df %>%
      dplyr::rename(StationName = "BioRegion")
    plotCols <- colCPR
    ltype <- "solid"

  } else if (Survey == "NRS" | Survey == "Coastal"){
    df <- df %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c("SampleTime_Local", "StationName", "Parameters"))) # accounting for microbial data different depths
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
#' df <- pr_get_Indices("NRS", "Zooplankton") %>%
#'   dplyr::filter(Parameters == "Biomass_mgm3") %>%
#'   pr_model_data()
#' pr_plot_Trends(df, method = "loess", Trend = "Month")
#' pr_plot_Trends(df, Trend = "Year")
#' pr_plot_Trends(df, Trend = "Raw")
pr_plot_Trends <- function(df, Trend = "Raw", method = "lm",  trans = "identity"){

  # Do one parameter at a time at the moment.
  assertthat::assert_that(
    nrow(df) > 0,
    msg = "No data in the dataframe. Check download or filtering."
  )

  Type <- pr_get_type(df)

  Survey <- pr_get_survey(df)

  # Extract Model data
  Models <- pr_get_model(df)

  if(is.null(Models)){ # TODO Decide if we will always provide models or not. Currently we are forcing models to be run and shown.
    df <- pr_model_data(df)
    Models <- pr_get_model(df)
  }

  if (Trend %in% c("Month", "Year")){
    Trend = paste0(Trend, "_Local") # Rename to match columns
  }

  # Set Correct columns/plot titles
  if (Survey == "CPR"){
    site <- rlang::sym("BioRegion")
  } else if (Survey != "CPR"){
    site <- rlang::sym("StationName")
  }

  ## Should Model data be used.
  if (Trend != 'Raw'){
    labels <- df %>%
      dplyr::select(tidyselect::all_of(site)) %>%
      dplyr::distinct() %>%
      tibble::deframe()

  } else {

    coefficients <- pr_get_coeffs(Models, id = as.character(site))

    labels <- coefficients %>%
      dplyr::filter(.data$term == "Year_Local") %>%
      dplyr::select(tidyselect::all_of(site), "p.value", "signif") %>%
      dplyr::mutate(p.value = dplyr::if_else(.data$p.value > 0.001, as.character(round(.data$p.value, 3)), format(.data$p.value, scientific = TRUE, digits = 3)),
                    facet_label = paste0(!!site, " (p = ", .data$p.value, .data$signif,")"),
                    facet_label = dplyr::if_else(stringr::str_detect(.data$facet_label, "Bonney"), "Bonney Coast", .data$facet_label)) %>%
      dplyr::select(tidyselect::all_of(site), "facet_label") %>%
      tibble::deframe()

  }

  titley <- pr_relabel(unique(df$Parameters), style = 'ggplot')

  # Averaging based on `Trend` or taking climatology from model for month

  if (Trend %in% c("Year_Local")){
    df <- df %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       # facet_label = dplyr::first(.data$facet_label),
                       .by = c(rlang::as_string(rlang::sym(Trend)), rlang::as_string(site)))

  } else if (Trend %in% c("Month_Local")){
    term_vals <- seq(0.524, 6.284, length.out = 12)
    means <- df %>%
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE), .by = c('Month_Local', rlang::as_string(site)))

    newdata <- data.frame(
      Month = term_vals,
      Year_Local = stats::median(df$Year_Local))

    df <- purrr::imap(Models, ~ predict(.x, newdata = newdata, se.fit = TRUE)) %>%
      dplyr::bind_rows(.id = as.character(site)) %>%
      planktonr_dat(Type = Type, Survey = Survey) %>%
      dplyr::mutate(Month = rep(term_vals, length(Models)),
                    Month_Local = round(rep(term_vals, length(Models)) * 12 / (3.142 * 2),0),
                    upper = .data$fit + 1.96*.data$se.fit,
                    lower = .data$fit -1.96*.data$se.fit) %>%
      dplyr::select(tidyselect::all_of(site), "Month_Local", "Month", "fit", "upper", "lower") %>%
      dplyr::left_join(means, by = c('Month_Local', as.character(site))) %>%
      pr_reorder()

  } else {
    Trend <- "SampleTime_Local"
  }

  # Remove smooth from VBM
  if (Survey == "NRS"){
    df <- df %>%
      dplyr::mutate(do_smooth = !!site != "Bonney Coast")
  } else {
    df <- df %>%
      dplyr::mutate(do_smooth = TRUE)
  }

  # Do the plotting
  if (rlang::as_string(Trend) %in% c("Month_Local")){
    labx = "Month"
    yvals <- 'fit'
  } else {
    labx = 'Year'
    yvals <- 'Values'
  }

  p1 <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!rlang::sym(Trend), y = .data$Values)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(data = df %>% dplyr::filter(.data$do_smooth), ggplot2::aes(x = !!rlang::sym(Trend), y = !!rlang::sym(yvals)),
                         method = method, formula = y ~ x) +
    ggplot2::facet_wrap(site, scales = "free_y", ncol = 1, labeller = ggplot2::labeller(!!site := labels)) +
    ggplot2::ylab(rlang::enexpr(titley)) +
    ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    theme_pr() +
    ggplot2::xlab(labx) +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(Trend) %in% c("Month_Local")){
    p1 <- p1 +
      ggplot2::geom_ribbon(data = df, ggplot2::aes(ymin = .data$lower, ymax = .data$upper), fill = 'grey', alpha = 0.5) +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12),
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                                  guide = ggplot2::guide_axis(check.overlap = FALSE))
  } else if (!rlang::as_string(Trend) %in% c("Month_Local", "Year_Local") & Survey != 'Coastal'){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")
  } else if (!rlang::as_string(Trend) %in% c("Month_Local", "Year_Local") & Survey == 'Coastal'){
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  }

  return(p1)
}

#' Plot single climatology
#'
#' @param df dataframe with specified time period, station code and parameter
#' @param Trend specified time period
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a climatology plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
#' dplyr::filter(Parameters == "PhytoBiomassCarbon_pgL", StationCode %in% c("NSI", "PHB"))
#'
#' monthly <- pr_plot_Climatology(df, Trend = "Month")
#'
#' df <- pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
#'         dplyr::filter(Parameters == "ZoopAbundance_m3")
#' annual <- pr_plot_Climatology(df, Trend = "Year")
pr_plot_Climatology <- function(df, Trend = "Month", trans = "identity"){

  Survey <- pr_get_survey(df)
  Type <- pr_get_type(df)
  Variable <- pr_get_variable(df)

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


  #TODO !! Doesn't work with the custom class so we convert to tibble and then back again. It would be interesting to find out why one day....
  df_climate <- df %>%
    # tibble::as_tibble() %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .by = tidyselect::all_of(c(rlang::as_string(Trend), "StationName"))) %>%
    tidyr::complete(!!Trend, .data$StationName) # %>%
    # planktonr_dat(Type = Type, Survey = Survey, Variable = Variable)


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
    ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_fill_manual(values = plotCols, limits = force, guide = ggplot2::guide_legend(byrow = TRUE)) +
    theme_pr()

  if("Month_Local" %in% colnames(df_climate)){
    p1 <- p1 +
      ggplot2::xlab("Month") +
      ggplot2::scale_x_continuous(breaks = seq(1,12, length.out = 12), expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
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
#' @param trans scale of y axis on plot, whatever scale_y_continuous trans accepts
#'
#' @return a combined plot
#' @export
#'
#' @examples
#' df <- pr_get_Indices(Survey = "CPR", Type = "Phytoplankton") %>%
#'   dplyr::filter(Parameters == "PhytoAbundance_Cellsm3")
#' pr_plot_tsclimate(df)
pr_plot_tsclimate <- function(df, trans = "identity"){

  Survey <- pr_get_survey(df)

  p1 <- pr_plot_TimeSeries(df, trans) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank())

  p2 <- pr_plot_Climatology(df, "Month", trans) +
    ggplot2::theme(legend.position = "none")

  p3 <- pr_plot_Climatology(df, "Year", trans) +
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
#' df <- pr_get_FuncGroups("NRS", "Phytoplankton") %>%
#' dplyr::filter(StationCode == 'PHB')
#' plot <- pr_plot_tsfg(df, "Actual", Trend = 'Month')
#' plot
pr_plot_tsfg <- function(df, Scale = "Actual", Trend = "Raw"){

  df <- tibble::as_tibble(df)

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
      dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                       N = dplyr::n(),
                       sd = sd(.data$Values, na.rm = TRUE),
                       se = sd / sqrt(.data$N),
                       .by = tidyselect::all_of(c(rlang::as_string(rlang::sym(Trend)), rlang::as_string(station), "Parameters")))

  } else {
    Trend <- SampleDate # Rename Trend to match the column with time
  }

  if(Scale == "Percent") {
    df <- df %>%
      dplyr::summarise(n = sum(.data$Values, na.rm = TRUE),
                       .by = tidyselect::all_of(c(rlang::as_string(rlang::sym(Trend)), rlang::as_string(station), "Parameters"))) %>%
      dplyr::mutate(Values = .data$n / sum(.data$n, na.rm = TRUE))
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
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust = 0))

  if (rlang::as_string(Trend) %in% c("Month_Local")){
    lims <- c(1, 12)
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                  limits = lims,
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month")
  } else if (rlang::as_string(Trend) %in% c("Year_Local")){
    p1 <- p1 +
      ggplot2::scale_x_continuous(breaks = 2, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
      ggplot2::xlab("Year")
  } else if (rlang::as_string(Trend) %in% c("SampleTime_Local")){
    lims <- as.POSIXct(strptime(c(min(df$SampleTime_Local),max(df$SampleTime_Local)), format = "%Y-%m-%d %H:%M"))
    p1 <- p1 +
      ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims, expand = ggplot2::expansion(add = c(0.15, 0.15))) +
      ggplot2::xlab("Sample Date")
  }

  return(p1)
}



#' Essential Ocean Variables plot
#'
#' @param df dataframe containing timeseries data with Parameters and Values, output of pr_get_EOVs and pr_get_Coeffs
#' @param EOV Essential Ocean Variable as a parameter
#' @param trans scale for y axis
#' @param col colour selection
#' @param labels do you want to print labels on the x axes
#'
#' @return plot of timeseries, anomalies and climatology
#' @export
#'
#' @examples
#' df <- pr_get_EOVs("NRS") %>%
#'   dplyr::filter(StationCode == "PHB") %>%
#'   pr_remove_outliers(2)
#' pr_plot_EOVs(df, EOV = "Biomass_mgm3",
#'       trans = "identity", col = "blue", labels = FALSE)
pr_plot_EOVs <- function(df, EOV = "Biomass_mgm3", trans = "identity", col = "blue", labels = TRUE) {

  # Ensure there is only one parameter
  df <- df %>%
    dplyr::filter(.data$Parameters == EOV)

  if (dim(df)[1] > 1){ # If only a few

    # Get the survey name
    Survey <- pr_get_survey(df)

    if (Survey == "CPR"){
      site = rlang::sym("BioRegion")
    } else if (Survey != "CPR"){
      site = rlang::sym("StationName")
    }

    # TODO need to add assert
    # Ensure there is only 1 station
    assertthat::assert_that(
      length(unique(df[[site]])) == 1,
      msg = "Only 1 Station/BioRegion can be plotted. Either filter the dataset or use purrr::map to process individually."
    )

    # TODO At the moment, the model parameters are still being put into columns.
    # I want to change this to be a model object in a slot that can be extracted by
    # a generic summary.planktonr_dat() call. I will need a different model object per variable.
    # It should update each time it is plotted and subset so it is accuate for the data contained
    # in the data frame. Consider using tidymodels to do it.

    # TODO Should I be only processing 1 station/Bioregion?

    df <- pr_model_data(df) %>%
      dplyr::mutate(Month = lubridate::month(.data$SampleTime_Local)  * 2 * 3.142 / 12 )

    # Extract Model data
    Models <- pr_get_model(df)

    coefficients <- pr_get_coeffs(Models, id = as.character(site)) %>%
      dplyr::filter(.data$term == "Year_Local") %>%
      dplyr::mutate(p.value = dplyr::if_else(.data$p.value > 0.001,
                                             as.character(round(.data$p.value, digits = 3)),
                                             format(.data$p.value, scientific = TRUE, digits = 3)))

    # plot monthly climatology from model
    # set up new data from predictions
    term_vals <- seq(0, 6.284, length.out = 24)

    newdata <- data.frame(
      Month = term_vals,
      Year_Local = stats::median(df$Year_Local))

    # extract monthly climatology data from model
    dfm <- purrr::imap(Models, ~ predict(.x, newdata = newdata, se.fit = TRUE)) %>%
      dplyr::bind_rows(.id = as.character(site)) %>%
      # data.frame() %>%
      dplyr::mutate(Month_Local = rep(term_vals, length(Models)),
                    upper = .data$fit + 1.96*.data$se.fit,
                    lower = .data$fit -1.96*.data$se.fit,
                    do_smooth = !!site != "Bonney Coast") %>%
      dplyr::select(tidyselect::all_of(site), "Month_Local", "do_smooth", Values = .data$fit, "upper", "lower")


    # The title comes back as class "call" so I need to undo and redo it to add the string
    titley <- pr_relabel(EOV, style = "ggplot") %>%
      as.list() %>%
      c(paste0(" [p = ",coefficients$p.value ,coefficients$signif, "]")) %>%
      as.call()

    lims <- c(lubridate::floor_date(min(df$SampleTime_Local), "year"),
              lubridate::ceiling_date(max(df$SampleTime_Local), "year"))
    df <- df %>%
      dplyr::filter(.data$Parameters == EOV)

    # Remove smooth from VBM
    if (Survey == "NRS"){
      df <- df %>%
        dplyr::mutate(do_smooth = !!site != "Bonney Coast")
    } else {
      df <- df %>%
        dplyr::mutate(do_smooth = TRUE)
    }

    p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleTime_Local, y = .data$Values)) +
      ggplot2::geom_point(colour = col) +
      ggplot2::geom_smooth(data = df %>% dplyr::filter(.data$do_smooth),
                           method = "lm", formula = y ~ x,
                           colour = col, fill = col, alpha = 0.4) +
      ggplot2::labs(x = "Year", subtitle = titley) +
      ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
      theme_pr() +
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank())

    if(isFALSE(labels)){
      p1 <- p1 +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }

    if(Survey == "LTM"){
      p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "10 years",
                                            date_labels = "%Y",
                                            limits = lims,
                                            expand = ggplot2::expansion(mult = c(0.02, 0.02)))
    } else {
      p1 <-  p1 + ggplot2::scale_x_datetime(date_breaks = "2 years",
                                            date_labels = "%Y",
                                            limits = lims,
                                            expand = ggplot2::expansion(mult = c(0.02, 0.02)))
    }



    p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$SampleTime_Local, .data$anomaly)) +
      ggplot2::geom_col(fill = col, colour = col, alpha = 0.5) +
      ggplot2::xlab("Year") +
      ggplot2::labs(y = "Anomaly") +
      theme_pr()


    if(isFALSE(labels)){
      p2 <- p2 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if(Survey == "LTM"){
      p2 <- p2 +
        ggplot2::scale_x_datetime(date_breaks = "10 years",
                                  date_labels = "%Y",
                                  limits = lims,
                                  expand = ggplot2::expansion(mult = c(0.02, 0.02)))
    } else {
      p2 <- p2 +
        ggplot2::scale_x_datetime(date_breaks = "2 years",
                                  date_labels = "%Y",
                                  limits = lims,
                                  expand = ggplot2::expansion(mult = c(0.02, 0.02)))
    }

    p3 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Month, y = .data$Values)) +
      ggplot2::geom_point(colour = col) +
      ggplot2::geom_smooth(data = dfm %>% dplyr::filter(.data$do_smooth), ggplot2::aes(x = .data$Month_Local, y = .data$Values),
                           method = "loess",
                           formula = "y ~ x", colour = col, fill = col, alpha = 0.4) +
      ggplot2::scale_y_continuous(trans = trans, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
      ggplot2::scale_x_continuous(breaks = seq(0.5, 6.3, length.out = 12), expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
      ggplot2::xlab("Month") +
      theme_pr() +
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank())

    if(isFALSE(labels)){
      p3 <- p3 +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }


  } else { # Blank plot for VBM for the moment

    p1 <- p2 <- p3 <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Not \nenough \ndata",
                        size = 6, color = "black", fontface = "bold",
                        hjust = 0.5, vjust = 0.5) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank()
      )

  }

  plots <- patchwork::wrap_plots(p1, p2, p3, widths = c(3,3,2))

  return(plots)


}



#' Plot of relative day and night abundances
#'
#' @param df dataframe as output of pr_get_DayNight() filtered for one species
#'
#' @return plot of relative day and night abundances
#' @export
#'
#' @examples
#' df <- pr_get_DayNight()
#' plot <- pr_plot_DayNight(df)
pr_plot_DayNight <-  function(df){

  titlemain <- unique(df$Species)
  ylabel <- rlang::expr(paste("Abundance (m"^-3,")"))

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
    dplyr::summarise(mean = mean(.data$Species_m3, na.rm = TRUE), .by = tidyselect::all_of("Project"))

  #means are so different so log data as the abundance scale is so wide

  sti <- df %>%
    dplyr::left_join(means, by = "Project") %>%
    dplyr::mutate(relab = .data$Species_m3/.data$mean) %>%
    dplyr::summarize(relab = sum(.data$relab),
                     freq = dplyr::n(),
                     a = sum(.data$relab)/dplyr::n(),
                     .by = tidyselect::all_of(c("SST", "Species")))

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



#' Plot for latitudinal data
#'
#' @param df dataframe of latitudinal series
#' @param na.fill TRUE, FALSE or function like mean to fill in gaps in data
#'
#' @return patchwork object
#' @export
#'
#' @examples
#' df <- pr_get_NRSMicro('GO-SHIP')
#' df <- df %>% dplyr::filter(Parameters == 'Bacteria_unique_ASVs',
#' SampleDepth_m < 101)
#' pr_plot_latitude(df, na.fill = mean)

pr_plot_latitude <- function(df, na.fill = TRUE){

  df <- df %>% tidyr::drop_na()

  param <- planktonr::pr_relabel(unique(df$Parameters), "ggplot")
  xlabel <- planktonr::pr_relabel('Latitude', "ggplot")
  Lab <- seq(round(min(df$Values), 0), round(max(df$Values), 0), length.out = 5)
  Breaks <- seq(round(min(df$Values), 0), round(max(df$Values), 0), length.out = 5)

  df1 <- df %>%
    dplyr::mutate(Values = ifelse(.data$Values < 0, 0, .data$Values),
                  Latitude = round(.data$Latitude, 0),
                  Label = .data$Latitude) %>%
    dplyr::arrange(.data$SampleDepth_m)

  gg <- ggplot2::ggplot(df1, ggplot2::aes(.data$Latitude, .data$SampleDepth_m, color = .data$Values)) +
    ggplot2::geom_point() +
    planktonr::theme_pr() +
    ggplot2::labs(y = "Depth (m)") +
    ggplot2::scale_color_continuous(type = "viridis") +
    ggplot2::scale_y_reverse()  +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    planktonr::theme_pr() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.position = 'none')

  df2 <- df1 %>%
    dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10, 0)*10)

  p1 <- ggplot2::ggplot() +
      metR::geom_contour_fill(data = df2, ggplot2::aes(x = .data$Latitude, y = .data$SampleDepth_m, z = .data$Values), na.fill = na.fill) +
      ggplot2::scale_fill_continuous(type = "viridis", name = param) +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$Latitude, y = .data$SampleDepth_m), size = 1) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_reverse(expand = c(0, 0)) +
      ggplot2::labs(y = "Depth (m)", x = xlabel) +
      planktonr::theme_pr() +
      ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0),
                   legend.position = 'bottom')

  plots <- patchwork::wrap_plots(gg, p1, ncol = 1)

  return(plots)

}
