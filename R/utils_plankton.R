#' Get functional group data
#' @param Survey CPR or NRS data
#' @param Type Zooplankton or phytoplankton data
#' @param ... to allow use of join when used within another function
#'
#' @return dataframe for plotting functional group time series info
#' @export
#'
#' @examples
#' NRSfgz <- pr_get_FuncGroups("NRS", "Z")
#' NRSfgp <- pr_get_FuncGroups("NRS", "P")
#' CPRfgz <- pr_get_FuncGroups("CPR", "Z", near_dist_km = 250)
#' CPRfgp <- pr_get_FuncGroups("CPR", "P")
pr_get_FuncGroups <- function(Survey = "NRS", Type = "Z", ...){

  if(Survey == "CPR"){
    df <- pr_get_CPRData(Type, Variable = "abundance", Subset = "htg") %>%
      pr_add_Bioregions(...)
  } else if(Survey == "NRS"){
    df <- pr_get_NRSData(Type, Variable = "abundance", Subset = "htg") %>%
      dplyr::filter(.data$StationName != "Port Hacking 4")
  }

  if(Type == "P"){
    var_names <- c("Centric diatom", "Ciliate", "Cyanobacteria", "Dinoflagellate", "Flagellate", "Foraminifera",
                   "Pennate diatom", "Radiozoa", "Silicoflagellate")
  } else if(Type == "Z"){
    var_names <- c("Ascidian", "Ciliate", "Ciliophora", "Coelenterate", "Ctenophore", "Echinoderm", "Euphausiid",
                   "Mysida", "Ostracod", "Phoronid", "Polychaete", "Urochordata", "Amphipod", "Appendicularian",
                   "Brachiopod", "Bryozoan", "Cephalochordate", "Cephalopod", "Chaetognath", "Cirripedia",
                   "Cladoceran", "Cnidarian", "Copepod", "Cumacean", "Decapod", "Egg", "Fish", "Foraminifera",
                   "Hemichordata", "Isopod", "Mollusc", "Nematode", "Nemertean", "Noctilucaceae", "Platyhelminthes",
                   "Radiozoa", "Rotifer", "Sipunculid", "Thaliacean", "Thecostraca")
  }

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::select("BioRegion", "SampleTime_Local", "Month_Local", "Year_Local", tidyselect::any_of(var_names)) %>%
      dplyr::filter(!is.na(.data$BioRegion), !.data$BioRegion %in% c("North", "North-west")) %>%
      droplevels()
  } else if(Survey == "NRS"){
    df <- df %>%
      dplyr::select("StationName", "StationCode", "SampleTime_Local",
                    "Month_Local", "Year_Local", tidyselect::any_of(var_names))
  }

  df <- df %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters")  %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    dplyr::filter(.data$Parameters != "Flagellate") %>%
    pr_reorder()

  if(Type == "P"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Centric diatom", "Pennate diatom", "Dinoflagellate", "Cyanobacteria"), .data$Parameters, "Other"),
                    Parameters = factor(.data$Parameters, levels = c("Centric diatom", "Pennate diatom", "Dinoflagellate", "Cyanobacteria",
                                                                     "Other")))
  } else if(Type == "Z"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean"), .data$Parameters, "Other"),
                    Parameters = factor(.data$Parameters, levels = c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean",
                                                                     "Other")))
  }

  df <- df %>%
    dplyr::group_by(dplyr::across(-"Values")) %>%
    dplyr::summarise(Values = sum(.data$Values, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Values = ifelse(.data$Values < 1, 1, .data$Values))

  return(df)
}



# Get the summary plankton observations
#
# Get the summary plankton observations from the NRS and CPR.
# @return a dataframe with a species summary
#
# @param Type The group of plankton requested. Either "Z" or "P"
#
# @export
#
# @examples
# df <- pr_get_SppCount("P")
# df <- pr_get_SppCount("Z")
# pr_get_SppCount <- function(Type = "Z"){
#
#   if (Type == "P"){
#     gp <- "Phytoplankton"
#   } else if (Type == "Z"){
#     gp <- "Zooplankton"
#   }
#
# out <- sppSummary %>%
#   dplyr::filter(.data$Group == gp)
#
# }



#' Data for PCI plot from CPR data
#'
#' @return dataframe of PCI data
#' @export
#'
#' @examples
#' head(pr_get_PCIData(),5)
pr_get_PCIData <- function(){
  dat <- planktonr::pr_get_Indices("CPR", "W", near_dist_km = 250) %>%
    dplyr::filter(.data$Parameters == 'PCI') %>%
    dplyr::mutate(Longitude = round(.data$Longitude, 0),
                  Latitude = round(.data$Latitude, 0),
                  Season = dplyr::case_when(.data$Month_Local > 2 & .data$Month_Local < 6 ~ "March - May",
                                            .data$Month_Local > 5 & .data$Month_Local < 9 ~ "June - August",
                                            .data$Month_Local > 8 & .data$Month_Local < 12 ~ "September - November",
                                            TRUE ~ "December - February")) %>%
    dplyr::group_by(.data$Longitude, .data$Latitude, .data$Season, .data$BioRegion) %>%
    dplyr::summarise(PCI = mean(.data$Values, na.rm = TRUE),
                     .groups = 'drop')

}



#' Get taxa accumulation data for plotting
#'
#' @param Survey "NRS or "CPR"
#' @param Type "Z" or "P"
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df <- pr_get_TaxaAccum(Survey = "NRS", Type = "Z")
pr_get_TaxaAccum <- function(Survey = "NRS", Type = "Z"){

  if (Survey == "NRS"){
    dat <- pr_get_NRSData(Type = Type, Variable = "abundance", Subset = "raw")
  } else if (Survey == "CPR"){
    dat <- pr_get_CPRData(Type = Type, Variable = "abundance", Subset = "raw")
  }

  dat <- dat %>%
    tidyr::pivot_longer(-pr_get_NonTaxaColumns(Survey = Survey, Type = Type), names_to = "Taxa", values_to = "Abundance") %>%
    dplyr::filter(.data$Abundance > 0) %>%
    dplyr::arrange(.data$SampleTime_Local) %>%
    dplyr::group_by(.data$Taxa) %>%
    dplyr::summarise(First = dplyr::first(.data$SampleTime_Local), .groups = "drop") %>%
    dplyr::arrange(.data$First) %>%
    dplyr::mutate(RowN = dplyr::row_number())
}






#' Get data for STI plots of species abundance
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return df to be sued with pr_plot_STI
#' @export
#'
#' @examples
#' df <- pr_get_STIdata("P")
#' df <- pr_get_STIdata("Z")
pr_get_STIdata <-  function(Type = "P"){

  if(Type == "Z"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "copepods")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "copepods")
    parameter <- "CopeAbundance_m3"

  } else if(Type == "P"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "species")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "species")
    parameter <- "PhytoAbundance_m3"
  }

  cprsat <- pr_get_SatData("CPR")
  nrssat <- pr_get_SatData("NRS")

  cpr <- cprdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(cprsat, by = c("Latitude", "Longitude", "SampleTime_UTC")) %>%
    dplyr::select("Species", "SST", tidyselect::all_of(parameter)) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "cpr",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  nrs <- nrsdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(nrssat, by = c("Latitude", "Longitude", "SampleTime_Local")) %>%
    dplyr::select("Species", "SST", tidyselect::all_of(parameter)) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "nrs",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  comball <- cpr %>%
    dplyr::bind_rows(nrs) %>%
    dplyr::mutate(SST = round(.data$SST/0.5) * 0.5) %>%
    dplyr::arrange(.data$Species)
}

#' Get STI kernel density for each species
#'
#' @param Type Zooplankton (Copepods) or phytoplankton species
#'
#' @return df of STI kernel density for each species
#' @export
#'
#' @examples
#' df <- pr_get_STI('P')
#'
pr_get_STI <-  function(Type = "Z"){
  df <- pr_get_STIdata(Type)

  species <- unique(df$Species)

  calc_sti <-  function(species){
    means <- df %>%
      dplyr::group_by(.data$Project) %>%
      dplyr::summarise(mean = mean(.data$Species_m3, na.rm = TRUE))

    #means are so different so log data as the abundance scale is so wide

    sti <- df %>%
      dplyr::filter(.data$Species == species) %>%
      dplyr::left_join(means, by = "Project") %>%
      dplyr::mutate(relab = .data$Species_m3/.data$mean) %>%
      dplyr::group_by(.data$SST, .data$Species) %>%
      dplyr::summarize(relab = sum(.data$relab),
                       freq = dplyr::n(),
                       a = sum(.data$relab)/dplyr::n(),
                       .groups = "drop")

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
                    density(SST, weight=weight,
                            bw=kernBw,
                            from=kernMin,
                            to=kernMax,
                            n=kernN))

    z <- data.frame(kernTemps, y = kernOut$y)
    STI <- round(z[which.max(z[,2]),]$kernTemps,1)
  }

  STI <- purrr::map(species, calc_sti)
  STI  <- data.frame(Species = species, STI = matrix(unlist(STI), nrow = length(STI), byrow = TRUE))

}

#' Get CTI for sample
#'
#' @param Type Zooplankton (Copepods) or phytoplankton species
#'
#' @return df of CTI for each sample
#' @export
#'
#' @examples
#' df <- pr_get_CTI("P")
#'
pr_get_CTI <-  function(Type = 'Z'){

  df <- pr_get_STI(Type)

  if(Type == 'Z'){
    dat <- pr_get_NRSData("zooplankton", "abundance", "species")
    vars <- pr_get_NonTaxaColumns(Survey = "NRS", Type)
  } else {
    dat <- pr_get_NRSData("phytoplankton", "abundance", "species")
    vars <- pr_get_NonTaxaColumns(Survey = "NRS", Type)
  }

  dat <- dat %>%
    dplyr::mutate(StationCode = ifelse(.data$StationCode == 'PH4', 'PHB', .data$StationCode)) %>%
    dplyr::select(-(tidyselect::any_of(vars)), "SampleTime_Local", "StationCode", "Project") %>%
    tidyr::pivot_longer(-c("SampleTime_Local", "StationCode", "Project"), values_to = "Abundance", names_to = "Species") %>%
    dplyr::inner_join(df, by = 'Species') %>%
    dplyr::group_by(.data$SampleTime_Local, .data$StationCode, .data$Project) %>%
    dplyr::summarise(CTI = sum(.data$Abundance * .data$STI, na.rm = TRUE)/sum(.data$Abundance, na.rm = TRUE),
                     .groups = "drop") %>%
    as.data.frame()

  return(dat)
}



#' Get data for plots of species abundance by day and night using CPR data
#' @param Type "P" or "Z" (default)
#'
#' @return df to be used with pr_plot_DayNight
#' @export
#'
#' @examples
#' df <- pr_get_DayNight(Type = "Z")
pr_get_DayNight <- function(Type = "Z"){

  if(Type == "Z"){
    dat <- pr_get_CPRData(Type = "Z", Variable = "abundance", Subset = "copepods")

  } else if (Type == "P"){
    dat <- pr_get_CPRData(Type = "P", Variable = "abundance", Subset = "species")

  }

  dates <- dat %>%
    dplyr::select("SampleTime_UTC", "Latitude", "Longitude") %>%
    dplyr::rename(date = "SampleTime_UTC",
                  lat = "Latitude",
                  lon = "Longitude") %>%
    dplyr::mutate(date = lubridate::as_date(.data$date))

  daynight_df <- suncalc::getSunlightTimes(data = dates, #TODO quicker to change to Local now that it exists.
                                           keep = c("sunrise", "sunset"),
                                           tz = "UTC") %>%
    dplyr::bind_cols(dat["SampleTime_UTC"]) %>%
    dplyr::mutate(daynight = ifelse(.data$SampleTime_UTC > .data$sunrise & .data$SampleTime_UTC < .data$sunset, "Day", "Night"))

  dat <- dat %>%
    dplyr::bind_cols(daynight_df["daynight"]) %>%
    dplyr::select(tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)), tidyselect::everything()) %>%
    tidyr::pivot_longer(-tidyselect::any_of(c(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type), "daynight")), values_to = "Species_m3", names_to = "Species") %>%
    dplyr::group_by(.data$Month_Local, .data$daynight, .data$Species) %>%
    dplyr::summarise(Species_m3 = mean(.data$Species_m3, na.rm = TRUE),
                     .groups = "drop")

}


# Add day/night marker to dataframe
# @param Type Phyto or zoo, defaults to phyto
#
# @return df to be used with pr_plot_DayNight
# @export
#
# @examples
# df <- pr_get_Indices(Survey = "NRS", Type = "Z")
# df <- pr_add_daynight(df)
# pr_add_daynight <- function(df){
#
#   dates <- df %>%
#     dplyr::select("SampleTime_Local", "Latitude", "Longitude") %>%
#     dplyr::rename(date = .data$SampleTime_Local,
#                   lat = .data$Latitude,
#                   lon = .data$Longitude) %>%
#     dplyr::mutate(date = lubridate::as_date(.data$date))
#
#   daynight_df <- suncalc::getSunlightTimes(data = dates, #TODO quicker to change to Local now that it exists.
#                                            keep = c("sunrise", "sunset"),
#                                            tz = lutz::tz_lookup_coords(dates$lat, dates$lon, method = "fast", warn = FALSE)) %>% # TODO remove this when we can get tz into all dataframes
#     dplyr::bind_cols(df["SampleTime_Local"]) %>%
#     dplyr::mutate(daynight = dplyr::if_else(.data$SampleTime_Local > .data$sunrise &
#                                       .data$SampleTime_Local < .data$sunset, "Day", "Night"))
#
#   df <- df %>%
#     dplyr::bind_cols(daynight_df["daynight"])
#
# }



#' Add Carbon concentration to phytoplankton dataframe
#'
#' This is where you write the description
#' @param df Input dataframe with BioVolume
#' @param meth Method for data collection
#'
#' @return Dataframe with Carbon included
#' @export
#'
#' @examples
#' df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
#'                           Biovolume_um3L = c(100, 150), Cells_L = c(10, 8))
#' df <- pr_add_Carbon(df, "NRS")
#' df <- data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
#'                           BioVolume_um3m3 = c(100, 150), PhytoAbund_m3 = c(10, 8))
#' df <- pr_add_Carbon(df, "CPR")
pr_add_Carbon <- function(df, meth){

  if (meth %in% "CPR"){
    df <- df %>%
      dplyr::mutate(BV_Cell = .data$BioVolume_um3m3 / .data$PhytoAbund_m3, # biovolume of one cell
                    Carbon = dplyr::case_when(.data$TaxonGroup == "Dinoflagellate" ~ 0.76*(.data$BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                              .data$TaxonGroup == 'Ciliate' ~ 0.22*(.data$BV_Cell)^0.939,
                                              .data$TaxonGroup == 'Cyanobacteria'~ 0.2,
                                              TRUE ~ 0.288*(.data$BV_Cell)^0.811),
                    Carbon_m3 = .data$PhytoAbund_m3 * .data$Carbon) # Carbon per m3
    return(df)
  }

  if (meth %in% "NRS"){
    df <- df %>%
      dplyr::mutate(BV_Cell = .data$Biovolume_um3L / .data$Cells_L, # biovolume of one cell
                    Carbon = dplyr::case_when(.data$TaxonGroup == "Dinoflagellate" ~ 0.76*(.data$BV_Cell)^0.819, # conversion to Carbon based on taxongroup and biovolume of cell
                                              .data$TaxonGroup == "Ciliate" ~ 0.22*(.data$BV_Cell)^0.939,
                                              .data$TaxonGroup == "Cyanobacteria" ~ 0.2,
                                              TRUE ~ 0.288*(.data$BV_Cell)^0.811),
                    Carbon_L = .data$Cells_L * .data$Carbon) # Carbon per litre
    return(df)
  }

}

