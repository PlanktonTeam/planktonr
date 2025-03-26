#
#
#
# # pr_model_data <- function(dat){
#
#
#   # TODO Need to ensure the models are calculated and stored against only 1
#   # parameter and 1 station. They seem to be returning results that are the same for all data regardless of location
#
#   # browser()
#
#   Survey <- pr_get_survey(df)
#   Type <- pr_get_type(df)
#   Variable <- pr_get_variable(df)
#
#   #
#   # if(Survey == "LTM") {
#   #   df <- df %>%
#   #     dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
#   #                      .by = tidyselect::all_of(c("StationCode", "StationName", "SampleTime_Local",
#   #                                                 "anomaly", "Year_Local", "Month_Local", "Parameters")))
#   # }
#
#   df <- df %>%
#     dplyr::rename(SampleDate = "SampleTime_Local") %>%
#     dplyr::mutate(Month = .data$Month_Local * 2 * 3.142 / 12) %>%
#     droplevels()
#
#
#   if(Survey == "CPR"){
#     stations <- df %>%
#       dplyr::select("BioRegion") %>%
#       unique()
#     stations <- stations$BioRegion
#   } else {
#     stations <- df %>%
#       dplyr::select("StationName") %>%
#       unique()
#     stations <- stations$StationName
#   }
#
#   params <- rep(params, each = length(stations))
#   stations <- rep(stations, length.out = length(params))
#
#   coeffs <- function(params, stations){
#
#     # browser()
#
#     lmdat <- df %>%
#       dplyr::filter(.data$Parameters == params) %>%
#       tidyr::drop_na()
#
#     if(Survey == "CPR"){
#       lmdat <- lmdat %>%
#         dplyr::filter(.data$BioRegion == stations)
#     } else {
#       lmdat <- lmdat %>%
#         dplyr::filter(.data$StationName == stations)
#     }
#
#     # If the combination of parameter and station doesn't exist, return NULL
#     if (dim(lmdat)[1] == 0){
#       return(NULL)
#     } else {
#
#       m <- stats::lm(Values ~ Year_Local + pr_harmonic(Month, k = 1), data = lmdat)
#
#       lmdat <- tibble::tibble(lmdat %>%
#                                 dplyr::bind_cols(fv = m$fitted.values))
#       ms <- summary(m)
#       slope <- ifelse(ms$coefficients[2,1] < 0, "decreasing", "increasing")
#       p <- ms$coefficients[2,4]
#       sig <- ifelse(ms$coefficients[2,4] < 0.05, "significantly", "but not significantly")
#
#       if(Survey != "CPR") { # NRS and LTM
#         dfs <- dplyr::tibble(slope = slope, p = p, significance = sig, Parameters = params, StationName = stations)
#         dfs2 <- lmdat %>%
#           dplyr::inner_join(dfs, by = c("Parameters", "StationName"))
#       } else {
#         dfs <- dplyr::tibble(slope = slope, p = p, significance = sig, Parameters = params, BioRegion = stations)
#         dfs2 <- lmdat %>%
#           dplyr::inner_join(dfs, by = c("Parameters", "BioRegion"))
#       }
#
#       # attr(dfs2, "Trend") <- m # Store the model object
#
#       return(dfs2)
#     }
#   }
#
#   # browser()
#
#   outputs <- purrr::map2(params, stations, coeffs) %>%
#     purrr::list_rbind() %>%
#     pr_planktonr_class(type = Type, survey = Survey, variable = Variable)
#
#   return(outputs)
#
#
#
#
#
#
# # } # End function
