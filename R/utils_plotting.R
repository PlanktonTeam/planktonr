
#' Prepare species occurrence data for frequency mapping
#'
#' @param Type Phytoplankton (P) or Zooplankton (Z), defaults to phyto
#'
#' @return dataframe for plotting with pr_plot_FreqMap
#' @export
#'
#' @examples
#' df <- pr_get_FreqMap("Zooplankton")

pr_get_FreqMap <- function(Type = "Zooplankton"){

  # Input validation
  assertthat::assert_that(
    is.character(Type) && length(Type) == 1,
    msg = "'Type' must be a single character string. Valid options are 'Phytoplankton' or 'Zooplankton'."
  )
  
  assertthat::assert_that(
    Type %in% c("Phytoplankton", "Zooplankton"),
    msg = "'Type' must be one of 'Phytoplankton' or 'Zooplankton'."
  )

  NRS <- pr_get_NRSData(Type = Type, Variable = "abundance", Subset = "species") %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = Type)),
                        names_to = "Species", values_to = "Counts")

  CountNRS <- NRS %>%
    dplyr::rename(Sample = "TripCode") %>%
    dplyr::mutate(Survey = 'NRS') %>%
    dplyr::select("Sample", "Survey", "Species", "Counts", "SampleTime_Local", "Month_Local", "Latitude", "Longitude")

  CPR <- pr_get_CPRData(Type = Type, Variable = "abundance", Subset = "species") %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)),
                        names_to = "Species", values_to = "Counts")

  CountCPR <- CPR %>%
    dplyr::rename(Sample = "Sample_ID") %>%
    dplyr::mutate(Survey = 'CPR') %>%
    dplyr::select("Sample", "Survey", "Species", "Counts", "SampleTime_Local", "Month_Local", "Latitude", "Longitude")

  dat <- dplyr::bind_rows(CountCPR, CountNRS)  %>%
    dplyr::mutate(Latitude = round(.data$Latitude/0.5, 0)*0.5,
                  Longitude = round(.data$Longitude/0.5, 0)*0.5,
                  Season = dplyr::case_when(.data$Month_Local > 2 & .data$Month_Local < 6 ~ "March - May",
                                            .data$Month_Local > 5 & .data$Month_Local < 9 ~ "June - August",
                                            .data$Month_Local > 8 & .data$Month_Local < 12 ~ "September - November",
                                            TRUE ~ "December - February"))

  totals <- dat %>%
    dplyr::select(-c('Species', 'Counts')) %>% # All samples including where nothing is counted
    dplyr::distinct() %>%
    dplyr::summarise(samples = dplyr::n(),
                     .by = tidyselect::all_of(c("Season", "Survey", "Latitude", "Longitude")))

  obs <- dat %>%  # Samples where something is counted
    dplyr::filter(.data$Counts > 0) %>%
    dplyr::summarise(freq = dplyr::n(),
                     .by = tidyselect::all_of(c("Season", "Survey", "Species", "Latitude", "Longitude"))) %>%
    dplyr::left_join(totals, by = c('Season', 'Survey', 'Latitude', 'Longitude')) %>%
    dplyr::mutate(freqsamp = .data$freq/.data$samples,
                  freqfac = as.factor(dplyr::case_when(.data$freqsamp < 0.375 ~ "Seen in 25%",
                                                       .data$freqsamp > 0.875 ~ "100% of Samples",
                                                       .data$freqsamp > 0.375 & .data$freqsamp < 0.625 ~ "50%",
                                                       TRUE ~ "75%")))

  # Adding empty samples back in for absences
  mapData <-  totals %>%
    dplyr::left_join(obs, by = c('Season', 'Survey', 'Latitude', 'Longitude', 'samples')) %>%
    dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Seen in 25%",'50%', '75%', '100% of Samples'))) %>%
    dplyr::arrange(.data$Species)

}



#' Prepare sampling coverage data for IMOS progress map visualisation
#'
#' @param Survey one of NRS, CPR or Both
#' @param interactive A logical TRUE/FALSE if the data is to be used for an interactive plot.
#' @param ... variables to be passed to pr_add_Bioregions. At the moment it only supports `near_dist_km` which is the distance (km) around each bioregion to pad the allocation of points.
#'
#' @return A dataframe for input into pr_plot_Progress()
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMapData(c("NRS", "CPR"))
#' df <- pr_get_ProgressMapData(c("NRS", "CPR"), interactive = TRUE)
pr_get_ProgressMapData <- function(Survey = c("NRS", "CPR"), interactive = FALSE, ...){

  # Input validation
  assertthat::assert_that(
    is.character(Survey),
    msg = "'Survey' must be a character vector. Valid options are 'NRS', 'CPR', or c('NRS', 'CPR')."
  )
  
  assertthat::assert_that(
    all(Survey %in% c("NRS", "CPR", "Both")),
    msg = "'Survey' must be one or more of 'NRS', 'CPR', or 'Both'."
  )
  
  assertthat::assert_that(
    is.logical(interactive) && length(interactive) == 1,
    msg = "'interactive' must be a single logical value (TRUE or FALSE)."
  )

  if (interactive == FALSE){
    if("NRS" %in% Survey) {
      PMapDataNRS <- planktonr::pr_get_NRSTrips() %>%
        dplyr::select("StationCode", "Longitude", "Latitude") %>%
        dplyr::rename(Region = "StationCode") %>%
        dplyr::mutate(Survey = "NRS") %>%
        dplyr::filter(.data$Region != "PH4")

      if (("CPR" %in% Survey) == FALSE){ # Return data if no CPR
        return(PMapDataNRS)
      }
    }

    if ("CPR" %in% Survey) {
      PMapDataCPR <- pr_get_s3("cpr_samp") %>%
        dplyr::select("REGION", "LONGITUDE", "LATITUDE") %>%
        pr_rename() %>%
        dplyr::mutate(Survey = "CPR")
      if (("NRS" %in% Survey) == FALSE){ # Return data if no NRS
        return(PMapDataCPR)
      }
    }

    if(("NRS" %in% Survey & "CPR" %in% Survey) | "Both" %in% Survey) {
      PMapData <- dplyr::bind_rows(PMapDataNRS, PMapDataCPR)
      return(PMapData)
    }
  } else if (interactive == TRUE){

    PMapDataNRS <- dplyr::bind_rows(planktonr::pr_get_Indices(Survey = "NRS", Type = "Zooplankton", ...),
                                    planktonr::pr_get_Indices(Survey = "NRS", Type = "Phytoplankton", ...)) %>%
      dplyr::filter(.data$Parameters == "ZoopAbundance_m3" | .data$Parameters == "PhytoAbundance_CellsL") %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values") %>%
      dplyr::rename(Name = "StationName") %>%
      dplyr::select(-"StationCode") %>%
      dplyr::mutate(Survey = "NRS")

    PMapDataCPR <- dplyr::bind_rows(planktonr::pr_get_Indices(Survey = "CPR", Type = "Zooplankton", ...),
                                    planktonr::pr_get_Indices(Survey = "CPR", Type = "Phytoplankton", ...)) %>%
      dplyr::filter(.data$Parameters == "ZoopAbundance_m3" |
                      .data$Parameters == "PhytoAbundance_Cellsm3" |
                      .data$Parameters == "PCI") %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values") %>%
      dplyr::mutate(PhytoAbundance_Cellsm3 = .data$PhytoAbundance_Cellsm3/1e3,
                    Survey = "CPR") %>%
      dplyr::rename(PhytoAbundance_CellsL = "PhytoAbundance_Cellsm3",
                    Name = "BioRegion")

    PMapData <- dplyr::bind_rows(PMapDataNRS, PMapDataCPR) %>%
      dplyr::select(-c("Year_Local", "Month_Local", "tz"))

    # Map colours for easy plotting
    PMapData <- PMapData %>%
      dplyr::left_join(mbr %>%
                         sf::st_drop_geometry(),
                       by = c("Name" = "REGION"))

    return(PMapData)
  }

}

